package mts.tasks

import mts.core._
import mts.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus

// TODO make this an actor to avoid non-thread-safe woes
// for now just make sure its stuff is only called inside its TaskManager
class NumAssignmentsHITManager[P, R](
  taskSpec: TaskSpecification { type Prompt = P ; type Response = R },
  numAssignmentsPerPrompt: Int,
  numHITsToKeepActive: Int,
  _promptSource: Iterator[P])(
  implicit pr: Reader[P],
  rr: Reader[R],
  config: TaskConfig
) extends HITManager[P, R](taskSpec) {

  import config._
  import Message._
  import taskSpec.hitTypeId

  private[this] val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  private[this] val activeHITs = {
    val active = mutable.Set.empty[HIT[Prompt]]
    for {
      mTurkHIT <- config.service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitTypeId)
      hit <- FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting
    } yield (active += hit)
    active
  }

  private[this] val savedDataByPrompt = {
    val result = mutable.Map.empty[Prompt, List[(HIT[Prompt], List[Assignment[Response]])]]
    FileManager.loadAllData[Prompt, Response](hitTypeId)
      .filter(hitInfo => !activeHITs.contains(hitInfo._1))
      .groupBy(_._1.prompt)
      .foreach { case (k, v) => result.put(k, v) }
    result
  }

  private[this] val (finishedPrompts, unfinishedPrompts) = {
    val finished = mutable.Set.empty[Prompt]
    val unfinished = mutable.Queue.empty[Prompt]
    savedDataByPrompt.foreach {
      case (prompt, hitInfos) =>
        if(hitInfos.map(_._2.size).sum >= numAssignmentsPerPrompt) {
          finished += prompt
        } else {
          unfinished.enqueue(prompt)
        }
    }
    (finished, unfinished)
  }

  override def refreshHITs: Unit = {
    // Right now just uploads new HITs as necessary.
    // TODO extend expired HITs?
    val numToUpload = numHITsToKeepActive - activeHITs.size
    for(_ <- 1 to numToUpload) {
      Try(unfinishedPrompts.dequeue).toOption match {
        case Some(nextPrompt) =>
          val assignmentsDone = savedDataByPrompt(nextPrompt).map(_._2.size).sum
          val assignmentsRemaining = numAssignmentsPerPrompt - assignmentsDone
          if(assignmentsRemaining <= 0) {
            // this shouldn't happen
            System.err.println("Thought prompt was unfinished when it was actually finished.")
            System.err.println(s"Prompt: $nextPrompt; HIT IDs: ${savedDataByPrompt(nextPrompt).map(_._1.hitId)}")
          } else {
            taskSpec.createHIT(nextPrompt, assignmentsRemaining) match {
              case Success(hit) =>
                activeHITs += hit
                println(s"Created HIT: ${hit.hitId}")
                println(s"https://workersandbox.mturk.com/mturk/preview?groupId=${hit.hitTypeId}")
              case Failure(e) =>
                System.err.println(e.getMessage)
                e.printStackTrace
                unfinishedPrompts.enqueue(nextPrompt) // try again later
            }
          }
        case None => queuedPrompts.pop match {
          case None => () // we're finishing off, woo
          case Some(nextPrompt) =>
            taskSpec.createHIT(nextPrompt, numAssignmentsPerPrompt) match {
              case Success(hit) =>
                activeHITs += hit
                println(s"Created HIT: ${hit.hitId}")
                println(s"https://workersandbox.mturk.com/mturk/preview?groupId=${hit.hitTypeId}")
              case Failure(e) =>
                System.err.println(e.getMessage)
                e.printStackTrace
                queuedPrompts.enqueue(nextPrompt) // put it back at the bottom to try later
            }
        }
      }
    }
  }

  /** Takes a reviewable HIT and does what must be done:
    * - trigger assignments to be reviewed.
    * - extend the hit if necessary.
    * - dispose of the hit if necessary.
    * - notify other tasks of results if necessary.
    * - upload new HITs if necessary.
    */
  final override def reviewHIT(taskActor: ActorRef, hit: HIT[Prompt]): Unit = {
    val submittedAssignments = service.getAllAssignmentsForHIT(hit.hitId, Array(AssignmentStatus.Submitted))
    if(submittedAssignments.isEmpty) {
      // must either have all assignments done or have expired. assume not expired, none rejected.
      // then we now have the desired number of assignments approved, and we can dispose of it.
      service.disposeHIT(hit.hitId)
      // remove from active, add to counts, and put into one of the inactive data structures
      activeHITs -= hit
      val curData = savedDataByPrompt.get(hit.prompt).getOrElse(Nil)
      val assignments = FileManager.loadAssignmentsForHIT[Response](hitTypeId, hit.hitId)
      val newData = (hit, assignments) :: curData
      savedDataByPrompt.put(hit.prompt, newData)
      val numAssignmentsCompleted = newData.map(_._2.size).sum
      if(numAssignmentsCompleted >= numAssignmentsPerPrompt) {
        finishedPrompts += hit.prompt
      } else {
        unfinishedPrompts += hit.prompt
      }
      refreshHITs
    } else {
      // when reviewing isn't so instant in the future,
      // we'll have to set HITs as reviewing, keep track of which assignments we need reviewed,
      // and then dispose the HIT once we have all of them reviewed.
      // service.setHITAsReviewing(hit.hitId)

      for(mTurkAssignment <- submittedAssignments) {
        val assignment = Assignment(
          hitTypeId = taskSpec.hitTypeId,
          hitId = hit.hitId,
          assignmentId = mTurkAssignment.getAssignmentId,
          workerId = mTurkAssignment.getWorkerId,
          acceptTime = mTurkAssignment.getAcceptTime.getTime.getTime,
          submitTime = mTurkAssignment.getSubmitTime.getTime.getTime,
          response = taskSpec.extractResponse(mTurkAssignment.getAnswer),
          feedback = taskSpec.extractFeedback(mTurkAssignment.getAnswer))
        reviewAssignment(taskActor, assignment)
      }
    }
  }

  private[this] def reviewAssignment(taskActor: ActorRef, assignment: Assignment[Response]): Unit = {
    taskActor ! EvaluateAssignment(assignment, Approval(""))
  }

  final override def addPrompt(prompt: Prompt): Unit = {
    queuedPrompts.enqueue(prompt)
    refreshHITs
  }
}
