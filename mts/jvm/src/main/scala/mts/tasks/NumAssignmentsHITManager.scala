package mts.tasks

import mts.core._
import mts.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

case class SetNumHITsActive(value: Int)

class NumAssignmentsHITManager[Prompt, Response](
  helper: HITManager.Helper[Prompt, Response],
  numAssignmentsPerPrompt: Int,
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

  private[this] val (finishedPrompts, unfinishedInactivePrompts) = {
    val finished = mutable.Set.empty[Prompt]
    val unfinished = mutable.Queue.empty[Prompt]
    finishedHITInfosByPromptIterator.foreach {
      case (prompt, hitInfos) =>
        if(hitInfos.map(_.assignments.size).sum >= numAssignmentsPerPrompt) {
          finished += prompt
        } else if(!activeHITInfos(prompt).isEmpty) {
          unfinished.enqueue(prompt)
        }
    }
    (finished, unfinished)
  }

  final override def reviewHITs: Unit = {
    for {
      mTurkHIT <- service.searchAllHITs().filter(_.getHITTypeId() == hitTypeId)
      hit <- FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting
    } yield {
      val submittedAssignments = service.getAllAssignmentsForHIT(hit.hitId, Array(AssignmentStatus.Submitted))
      // review all submitted assignments (that are not currently in review)
      for(a <- submittedAssignments) {
        val assignment = taskSpec.makeAssignment(hit.hitId, a)

        if(isInReview(assignment).isEmpty) {
          reviewAssignment(hit, assignment)
        }
      }
      // if the HIT is "reviewable", and all its assignments are reviewed (i.e., no longer "Submitted"), we can dispose
      if(mTurkHIT.getHITStatus == HITStatus.Reviewable && submittedAssignments.isEmpty) {
        finishHIT(hit)
        val numAssignmentsCompleted = finishedHITInfos(hit.prompt).map(_.assignments.size).sum
        if(numAssignmentsCompleted >= numAssignmentsPerPrompt) {
          finishedPrompts += hit.prompt
          promptFinished(hit.prompt)
        } else {
          unfinishedInactivePrompts += hit.prompt
        }
      }
    }

    // refresh: upload new hits to fill gaps
    val numToUpload = numHITsToKeepActive - numActiveHITs
    for(_ <- 1 to numToUpload) {
      Try(unfinishedInactivePrompts.dequeue).toOption match {
        case Some(nextPrompt) =>
          val assignmentsDone = finishedHITInfos(nextPrompt).map(_.assignments.size).sum
          val assignmentsRemaining = numAssignmentsPerPrompt - assignmentsDone
          if(assignmentsRemaining <= 0) {
            // this shouldn't happen
            System.err.println("Thought prompt was unfinished when it was actually finished.")
            System.err.println(s"Prompt: $nextPrompt; HIT IDs: ${finishedHITInfos(nextPrompt).map(_.hit.hitId)}")
          } else {
            createHIT(nextPrompt, assignmentsRemaining) recover {
              case _ => unfinishedInactivePrompts.enqueue(nextPrompt) // try again later
            }
          }
        case None => queuedPrompts.filterPop(p => !finishedPrompts.contains(p)) match {
          case None => () // we're finishing off, woo
          case Some(nextPrompt) =>
            createHIT(nextPrompt, numAssignmentsPerPrompt) recover {
              case _ => queuedPrompts.enqueue(nextPrompt) // put it back at the bottom to try later
            }
        }
      }
    }

    println(s"${queuedPrompts.numManuallyEnqueued} buffered load for HIT type $hitTypeId")
    println(s"$numActiveHITs active out of $numHITsToKeepActive for HIT type $hitTypeId")
  }
}
