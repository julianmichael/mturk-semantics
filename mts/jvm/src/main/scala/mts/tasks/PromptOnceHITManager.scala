package mts.tasks

import mts.core._
import mts.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus

import java.util.concurrent.atomic.AtomicInteger

/** Simple example DataManager.
  *
  * Runs through all of the prompts in the source, skipping ones that have already been finished
  * or are still active.
  * When a prompt fails, puts it at the back of the queue to try again at the end.
  *
  * @tparam Prompt the data representation of an MTurk question
  * @tparam Response the data representation of an annotator's response
  * @param hitType the HIT Type ID of the task this is managing data for
  * @param _promptSource iterator over the desired prompts to turn into questions
  * @param _finishedPrompts iterator over prompts that have already been completed and/or should be skipped
  */
class PromptOnceHITManager[P, R](
  taskSpec: TaskSpecification{ type Prompt = P; type Response = R },
  numAssignmentsPerHIT: Int,
  numHITsToKeepActive: Int,
  _promptSource: Iterator[P],
  _promptsToSkip: Iterator[P]
)(
  implicit pr: Reader[P],
  config: TaskConfig
) extends HITManager[P, R](taskSpec) {

  import config._
  import Message._

  private[this] val hitTypeId = taskSpec.hitTypeId

  private[this] val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  // private[this] val finishedPrompts: Set[Prompt] = ???

  // private[this] val unfinishedInactiveHITs: Set[HIT] = ???

  // private[this] val activeHITs: Set[HIT] = ???

  // using atomic integer for now; later ideally would make this class into an actor and remove the need
  private[this] val (finishedOrActivePrompts, numActiveHITs) = {
    val set = mutable.Set.empty[Prompt]
    set ++= _promptsToSkip
    val activePrompts = for {
      mTurkHIT <- config.service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitTypeId)
      hit = FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting.get
    } yield hit.prompt
    set ++= activePrompts
    (set, new AtomicInteger(activePrompts.size))
  }

  override def refreshHITs: Unit = {
    val numToUpload = numHITsToKeepActive - numActiveHITs.get()
    for(_ <- 1 to numToUpload) {
      val nextPromptOpt = queuedPrompts.pop
      nextPromptOpt match {
        case None => numActiveHITs.decrementAndGet()
        case Some(nextPrompt) =>
          taskSpec.createHIT(nextPrompt, numAssignmentsPerHIT) match {
            case Success(hit) =>
              numActiveHITs.incrementAndGet()
              println(s"Created HIT: ${hit.hitId}")
              println(s"You can view it here: https://workersandbox.mturk.com/mturk/preview?groupId=${hit.hitTypeId}")
            case Failure(e) =>
              System.err.println(e.getMessage)
              e.printStackTrace
              queuedPrompts.enqueue(nextPrompt) // put it back at the bottom to try later
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
      numActiveHITs.decrementAndGet()
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
