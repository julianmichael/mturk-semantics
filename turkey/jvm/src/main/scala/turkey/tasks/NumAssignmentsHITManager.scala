package turkey
package tasks

import turkey.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

case class SetNumHITsActive(value: Int)

object NumAssignmentsHITManager {
  def constAssignments[Prompt, Response](
    helper: HITManager.Helper[Prompt, Response],
    numAssignmentsPerPrompt: Int,
    initNumHITsToKeepActive: Int,
    _promptSource: Iterator[Prompt]) = new NumAssignmentsHITManager[Prompt, Response](
    helper, _ => numAssignmentsPerPrompt, initNumHITsToKeepActive, _promptSource)
}
class NumAssignmentsHITManager[Prompt, Response](
  helper: HITManager.Helper[Prompt, Response],
  numAssignmentsForPrompt: Prompt => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[Prompt]) extends HITManager[Prompt, Response](helper) {

  var numHITsToKeepActive: Int = initNumHITsToKeepActive

  def receiveAux2: PartialFunction[Any, Unit] =
    PartialFunction.empty[Any, Unit]

  override lazy val receiveAux: PartialFunction[Any, Unit] = (
    { case SetNumHITsActive(n) => numHITsToKeepActive = n }: PartialFunction[Any, Unit]
  ) orElse receiveAux2

  import helper._
  import config._
  import taskSpec.hitTypeId

  // override for more interesting review policy
  def reviewAssignment(hit: HIT[Prompt], assignment: Assignment[Response]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      println(s"Feedback: ${assignment.feedback}")
    }
  }

  // override to do something interesting after a prompt finishes
  def promptFinished(prompt: Prompt): Unit = ()

  // override if you want fancier behavior
  override def addPrompt(prompt: Prompt): Unit = {
    queuedPrompts.enqueue(prompt)
  }

  val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  def isFinished(prompt: Prompt) =
    finishedHITInfos(prompt).map(_.assignments.size).sum >= numAssignmentsForPrompt(prompt)

  final override def reviewHITs: Unit = {
    for {
      allMTurkHITs <- Try(service.searchAllHITs()).toOptionPrinting.toList
      mTurkHIT <- allMTurkHITs.filter(_.getHITTypeId() == hitTypeId)
      hit <- hitDataService.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting
    } yield {
      val submittedAssignments = service.getAllAssignmentsForHIT(hit.hitId, Array(AssignmentStatus.Submitted))
      // review all submitted assignments (that are not currently in review)
      for(a <- submittedAssignments) {
        val assignmentTry = Try(taskSpec.makeAssignment(hit.hitId, a))

        assignmentTry match {
          case Success(assignment) => if(isInReview(assignment).isEmpty) {
            reviewAssignment(hit, assignment)
          }
          case Failure(e) =>
            System.err.println("Error parsing assignment:")
            e.printStackTrace
            service.approveAssignment(
              a.getAssignmentId,
              "There was an error in parsing your response to the HIT. Please notify the requester.")
        }
      }
      // if the HIT is "reviewable", and all its assignments are reviewed (i.e., no longer "Submitted"), we can dispose
      if(mTurkHIT.getHITStatus == HITStatus.Reviewable && submittedAssignments.isEmpty) {
        finishHIT(hit)
        if(isFinished(hit.prompt)) {
          promptFinished(hit.prompt)
        }
      }
    }

    // refresh: upload new hits to fill gaps
    val numToUpload = numHITsToKeepActive - numActiveHITs
    for(_ <- 1 to numToUpload) {
      queuedPrompts.filterPop(p => !isFinished(p)) match {
        case None => () // we're finishing off, woo
        case Some(nextPrompt) =>
          if(isActive(nextPrompt)) {
            // if this prompt is already active, queue it for later
            // TODO probably want to delay it by a constant factor instead
            queuedPrompts.enqueue(nextPrompt)
          } else createHIT(nextPrompt, numAssignmentsForPrompt(nextPrompt)) recover {
            case _ => queuedPrompts.enqueue(nextPrompt) // put it back at the bottom to try later
          }
      }
    }
  }
}
