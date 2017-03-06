package mts.experiments.expE

import mts.tasks._
import mts.core._
import mts.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus

class QAGenHITManager(
  helper: HITManager.Helper[QAGenPrompt, QAGenResponse],
  numAssignmentsPerPrompt: Int,
  numHITsToKeepActive: Int,
  _promptSource: Iterator[QAGenPrompt]
) extends NumAssignmentsHITManager[QAGenPrompt, QAGenResponse](
  helper, numAssignmentsPerPrompt, numHITsToKeepActive, _promptSource) {

  import helper._
  import config._
  import taskSpec.hitTypeId

  override def reviewAssignment(hit: HIT[QAGenPrompt], assignment: Assignment[QAGenResponse]): Unit = {
    val numQAPairs = assignment.response.qaPairs.filter {
      case (question, answerSet) => !question.isEmpty && !answerSet.isEmpty
    }.size
    val totalBonus = bonuses.take(numQAPairs).sum
    helper.evaluateAssignment(helper.startReviewing(assignment), Approval(""))
    println(s"Approved HIT: ${assignment.hitId}")
    if(!assignment.feedback.isEmpty) {
      println(s"Feedback: ${assignment.feedback}")
    }
    if(totalBonus > 0.0) {
      println(s"Number of QA pairs: $numQAPairs. Bonus granted: $totalBonus")
      service.grantBonus(assignment.workerId, totalBonus, assignment.assignmentId,
                         s"""$numQAPairs question-answer pairs given.""")

    }
  }

}
