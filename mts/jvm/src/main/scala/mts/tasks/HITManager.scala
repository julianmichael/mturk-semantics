package mts.tasks

import mts.core._
import mts.util._

import com.amazonaws.mturk.requester.AssignmentStatus

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef

import upickle.default._

// TODO get rid of reviewy complexity and stuff; just implement next task's logic
abstract class HITManager[Prompt, Response](
  helper: HITManager.Helper[Prompt, Response]
) extends Actor {

  import helper.Message._
  import helper._

  final override def receive = receiveHelperMessage orElse receiveAux

  private[this] final val receiveHelperMessage: PartialFunction[Any, Unit] = {
    case DisableAll => disableAll
    case ReviewHITs => reviewHITs
    case AddPrompt(p) => addPrompt(p)
  }

  // can override if you want to process more kinds of messages
  def receiveAux: PartialFunction[Any, Unit] =
    PartialFunction.empty[Any, Unit]

  def reviewHITs: Unit
  def addPrompt(prompt: Prompt): Unit
}

// TODO is there some way of restricting access to the methods here
// so they aren't public but they are accessible by subclasses of HITManager?
// protected[HITManager] does not work.
object HITManager {
  class Helper[P, R](
    val taskSpec: TaskSpecification { type Prompt = P ; type Response = R })(
    implicit val promptReader: Reader[P],
    val responseReader: Reader[R],
    val responseWriter: Writer[R],
    val config: TaskConfig
  ) {
    private type Prompt = P
    private type Response = R

    import scala.collection.mutable

    object Message {
      sealed trait Message
      case object DisableAll extends Message
      case object ReviewHITs extends Message
      case class AddPrompt(prompt: Prompt) extends Message
    }
    import Message._
    import config._
    import taskSpec.hitTypeId

    // disable method, not really complete yet

    def disableAll: Unit = {
      // TODO get all currently pending assignments and manually approve them?
      // saving the results in another location? THEN disable all HITs?
      // NOTE the above is an old todo. not sure if I still want to do it. not taking the time to think about it
      // TODO XXX integrate this with helper state. as of now weird things prob will happen
      // so you need to restart any time you disable. fortunately you probably want to anyway...
      service.searchAllHITs()
        .filter(hit => hit.getHITTypeId().equals(hitTypeId))
        .foreach(hit => {
                   service.disableHIT(hit.getHITId())
                   println
                   println(s"Disabled HIT: ${hit.getHITId()}")
                   println(s"HIT type for disabled HIT: ${hitTypeId}")
                 })
    }

    // HITs Active stuff

    private[this] val activeHITs = {
      val active = mutable.Set.empty[HIT[Prompt]]
      for {
        mTurkHIT <- config.service.searchAllHITs
        if mTurkHIT.getHITTypeId.equals(hitTypeId)
        hit <- FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting
      } yield (active += hit)
      active
    }

    private[this] val (finishedHITInfosByPrompt, activeHITInfosByPrompt) = {
      val finishedRes = mutable.Map.empty[Prompt, List[HITInfo[Prompt, Response]]]
      val activeRes = mutable.Map.empty[Prompt, List[HITInfo[Prompt, Response]]]
      FileManager.loadAllHITInfo[Prompt, Response](hitTypeId)
        .groupBy(_.hit.prompt)
        .foreach { case (prompt, infos) =>
          infos.foreach { hitInfo =>
            if(activeHITs.contains(hitInfo.hit)) {
              activeRes.put(prompt, hitInfo :: activeRes.get(prompt).getOrElse(Nil))
            } else {
              finishedRes.put(prompt, hitInfo :: activeRes.get(prompt).getOrElse(Nil))
            }
          }
      }
      (finishedRes, activeRes)
    }

    def finishedHITInfosByPromptIterator: Iterator[(Prompt, List[HITInfo[Prompt, Response]])] =
      finishedHITInfosByPrompt.iterator
    def finishedHITInfos(p: Prompt): List[HITInfo[Prompt, Response]] =
      finishedHITInfosByPrompt.get(p).getOrElse(Nil)
    def activeHITInfosByPromptIterator: Iterator[(Prompt, List[HITInfo[Prompt, Response]])] =
      activeHITInfosByPrompt.iterator
    def activeHITInfos(p: Prompt): List[HITInfo[Prompt, Response]] =
      activeHITInfosByPrompt.get(p).getOrElse(Nil)
    def allCurrentHITInfosByPromptIterator: Iterator[(Prompt, List[HITInfo[Prompt, Response]])] =
      activeHITInfosByPromptIterator ++ finishedHITInfosByPromptIterator
    def allCurrentHITInfos(p: Prompt): List[HITInfo[Prompt, Response]] =
      activeHITInfos(p) ++ finishedHITInfos(p)

    def createHIT(prompt: Prompt, numAssignments: Int): Try[HIT[Prompt]] = {
      val attempt = taskSpec.createHIT(prompt, numAssignments)
      attempt match {
        case Success(hit) =>
          activeHITs += hit
          println(s"Created HIT: ${hit.hitId}")
          println(s"${service.getWebsiteURL}/mturk/preview?groupId=${hit.hitTypeId}")
        case Failure(e) =>
          System.err.println(e.getMessage)
          e.printStackTrace
      }
      attempt
    }

    def isActive(hit: HIT[Prompt]): Boolean = activeHITs.contains(hit)
    def isActive(hitId: String): Boolean = activeHITs.exists(_.hitId == hitId)
    def numActiveHITs = activeHITs.size

    def finishHIT(hit: HIT[Prompt]): Unit = {
      service.disposeHIT(hit.hitId)
      activeHITs -= hit
      // add to other appropriate data structures
      val finishedData = finishedHITInfosByPrompt.get(hit.prompt).getOrElse(Nil)
      val activeData = activeHITInfosByPrompt.get(hit.prompt).getOrElse(Nil)
      val curInfo = activeData
        .find(_.hit.hitId == hit.hitId)
        .getOrElse {
        System.err.println("Warning: could not find active HIT to move to finished");
        HITInfo(
          hit,
          FileManager.loadAssignmentsForHIT[Response](hitTypeId, hit.hitId))
      }
      val newActiveData = activeData.filterNot(_.hit.hitId == hit.hitId)
      val newFinishedData = curInfo :: finishedData
      activeHITInfosByPrompt.put(hit.prompt, newActiveData)
      finishedHITInfosByPrompt.put(hit.prompt, newFinishedData)
    }

    // Assignment reviewing

    class AssignmentInReview protected[Helper] (val assignment: Assignment[Response])

    private[this] val assignmentsInReview = mutable.Set.empty[AssignmentInReview]

    def isInReview(assignment: Assignment[Response]): Option[AssignmentInReview] =
      assignmentsInReview.find(_.assignment == assignment)
    def isInReview(assignmentId: String): Option[AssignmentInReview] =
      assignmentsInReview.find(_.assignment.assignmentId == assignmentId)
    def numAssignmentsInReview = assignmentsInReview.size

    def startReviewing(assignment: Assignment[Response]): AssignmentInReview = {
      val aInRev = new AssignmentInReview(assignment)
      assignmentsInReview += aInRev
      aInRev
    }
    def evaluateAssignment(
      hit: HIT[Prompt],
      aInRev: AssignmentInReview,
      evaluation: AssignmentEvaluation
    ): Unit = {
      import aInRev.assignment
      evaluation match {
        case Approval(message) =>
          service.approveAssignment(assignment.assignmentId, message)
          assignmentsInReview -= aInRev
          val curData = activeHITInfosByPrompt.get(hit.prompt).getOrElse(Nil)
          val curInfo = curData.find(_.hit.prompt == hit.prompt)
            .getOrElse(HITInfo[Prompt, Response](hit, Nil))
          val filteredData = curData.filterNot(_.hit.prompt == hit.prompt)
          val newInfo = curInfo.copy(assignments = assignment :: curInfo.assignments)
          activeHITInfosByPrompt.put(hit.prompt, newInfo :: filteredData)
          println
          println(s"Approved assignment: ${assignment.assignmentId}")
          println(s"HIT for approved assignment: ${assignment.hitId}; $hitTypeId")
          FileManager.saveApprovedAssignment(assignment).recover { case e =>
            e.printStackTrace
            System.err.println(s"Failed to save approved assignment; data:")
            System.err.println(write(assignment))
        }
        case Rejection(message) =>
          service.rejectAssignment(assignment.assignmentId, message)
          assignmentsInReview -= aInRev
          println
          println(s"Rejected assignment: ${assignment.assignmentId}")
          println(s"HIT for rejected assignment: ${assignment.hitId}; $hitTypeId")
          println(s"Reason: $message")
          FileManager.saveRejectedAssignment(assignment) recover { case e =>
            e.printStackTrace
            System.err.println(s"Failed to save approved assignment; data:")
            System.err.println(write(assignment))
          }
      }
    }
  }
}

