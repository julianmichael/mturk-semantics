package mts.experiments.expH

import mts.core._
import mts.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

// class ReactiveSpecialWordHITManager(
//   helper: HITManager.Helper[Prompt, Response],
//   validationActor: ActorRef,
//   initNumHITsToKeepActive: Int,
//   _promptSource: Iterator[Prompt]) extends HITManager[GenerationPrompt, GenerationResponse](helper) {

//   import helper._
//   import config._
//   import taskSpec.hitTypeId

//   var numHITsToKeepActive: Int = initNumHITsToKeepActive

//   override def receiveAux = {
//     // TODO receive orders to increase num hits active
//     // TODO get validation results here
//     // evaluateAssignment(, Approval(""))
//   }

//   // could add a few sanity checks... meh
//   def reviewAssignment(assignment: Assignment[Response]): Unit = {
//     // check that no two questions are identical? eh, let's not...
//     evaluateAssignment(startReviewing(assignment), Approval(""))
//     // TODO send questions for validation
//   }

//   // updated when we get validation results
//   val completedWordStatsBySentenceId = {
//     val res = mutable.Map.empty[SentenceId, WordStats]
//       ???

//     res
//   }

//   // updated when we get responses or validation results
//   val tentativeWordStatsBySentenceId = {
//     val res = mutable.Map.empty[SentenceId, WordStats]
//       ???

//     res
//   }

//   private[this] val queuedSentenceIds = new LazyStackQueue[SentenceId](_promptSource)

//   private[this] val (finishedPrompts, unfinishedInactivePrompts) = {
//     val finished = mutable.Set.empty[Prompt]
//     val unfinished = mutable.Queue.empty[Prompt]
//     finishedAssignmentsByPromptIterator.foreach {
//       case (prompt, hitInfos) =>
//         if(hitInfos.map(_._2.size).sum >= numAssignmentsPerPrompt) {
//           finished += prompt
//         } else {
//           unfinished.enqueue(prompt)
//         }
//     }
//     (finished, unfinished)
//   }

//   final override def reviewHITs: Unit = {
//     for {
//       mTurkHIT <- service.searchAllHITs().filter(_.getHITTypeId() == hitTypeId)
//       hit <- FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting
//     } yield {
//       val submittedAssignments = service.getAllAssignmentsForHIT(hit.hitId, Array(AssignmentStatus.Submitted))
//       // review all submitted assignments (that are not currently in review)
//       for(a <- submittedAssignments) {
//         val assignment = taskSpec.makeAssignment(hit.hitId, a)

//         if(isInReview(assignment).isEmpty) {
//           reviewAssignment(assignment)
//         }
//       }
//       // if the HIT is "reviewable", and all its assignments are reviewed (i.e., no longer "Submitted"),
//       // we choose whether to EXTEND: TODO
//       if(mTurkHIT.getHITStatus == HITStatus.Reviewable && submittedAssignments.isEmpty) {
//         finishHIT(hit)
//         val numAssignmentsCompleted = finishedAssignments(hit.prompt).map(_._2.size).sum
//         if(numAssignmentsCompleted >= numAssignmentsPerPrompt) {
//           finishedPrompts += hit.prompt
//         } else {
//           unfinishedInactivePrompts += hit.prompt
//         }
//       }
//     }

//     // refresh: upload new hits to fill gaps
//     val numToUpload = numHITsToKeepActive - numActiveHITs
//     for(_ <- 1 to numToUpload) {
//       Try(unfinishedInactivePrompts.dequeue).toOption match {
//         case Some(nextPrompt) =>
//           val assignmentsDone = finishedAssignments(nextPrompt).map(_._2.size).sum
//           val assignmentsRemaining = numAssignmentsPerPrompt - assignmentsDone
//           if(assignmentsRemaining <= 0) {
//             // this shouldn't happen
//             System.err.println("Thought prompt was unfinished when it was actually finished.")
//             System.err.println(s"Prompt: $nextPrompt; HIT IDs: ${finishedAssignments(nextPrompt).map(_._1.hitId)}")
//           } else {
//             createHIT(nextPrompt, assignmentsRemaining) recover {
//               case _ => unfinishedInactivePrompts.enqueue(nextPrompt) // try again later
//             }
//           }
//         case None => queuedPrompts.filterPop(p => !finishedPrompts.contains(p)) match {
//           case None => () // we're finishing off, woo
//           case Some(nextPrompt) =>
//             createHIT(nextPrompt, numAssignmentsPerPrompt) recover {
//               case _ => queuedPrompts.enqueue(nextPrompt) // put it back at the bottom to try later
//             }
//         }
//       }
//     }
//   }

//   // shouldn't need this but oh well
//   final override def addPrompt(prompt: Prompt): Unit = {
//     queuedPrompts.enqueue(prompt)
//   }
// }
