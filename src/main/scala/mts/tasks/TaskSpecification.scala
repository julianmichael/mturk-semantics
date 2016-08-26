package mts.tasks

import mts.core._
import mts.util._

import com.amazonaws.mturk.requester.{HIT => MTurkHIT}
import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.requester.ReviewPolicy
import com.amazonaws.mturk.requester.PolicyParameter
import com.amazonaws.mturk.requester.{Assignment => MTurkAssignment}
import com.amazonaws.mturk.requester.AssignmentStatus

import java.util.Calendar

import scala.util.Try

import upickle.default.Writer

trait TaskSpecification[Prompt, Response] {
  import Config._

  // HIT type fields

  val title: String
  val description: String
  val keywords: String
  val reward: Double
  val autoApprovalDelay = 3600L // seconds (1 hour)
  val assignmentDuration = 600L // seconds (10 minutes)
  val qualRequirements = Array.empty[QualificationRequirement]

  // other fields

  val numAssignmentsPerHIT: Int
  val lifetime = 2592000L // seconds (30 days)

  // QA specification methods

  def createQuestionXML(prompt: Prompt): String
  def extractResponse(answerXML: String): Response
  def extractFeedback(answerXML: String): String

  // final members and methods

  // really not gonna bother with Amazon's automatic review policies for anything
  final val assignmentReviewPolicy = null
  final val hitReviewPolicy = null

  // not 100% sure this needs to be lazy but since it makes a server call I thought might as well
  final lazy val hitType = service.registerHITType(
    autoApprovalDelay,
    assignmentDuration,
    reward,
    title,
    keywords,
    description,
    qualRequirements)

  final def createHIT(prompt: Prompt)(implicit w: Writer[Prompt]): Try[HIT[Prompt]] = {
    val questionXML = createQuestionXML(prompt)

    // just hash the time and main stuff of our request for the unique token.
    val uniqueRequestToken = (hitType, questionXML, System.nanoTime()).toString.hashCode.toString

    for {
      mTurkHIT <- Try(
        service.createHIT(
          hitType,
          title,
          description,
          keywords,
          questionXML,
          reward,
          assignmentDuration,
          autoApprovalDelay,
          lifetime,
          numAssignmentsPerHIT,
          "", // don't bother with annotation---we don't get it back and it causes errors if >255 bytes (which was documented NOWHERE)
          qualRequirements,
          Array("Minimal", "HITQuestion", "HITDetail"), // response groups --- these don't actually do anything :(
          uniqueRequestToken,
          assignmentReviewPolicy,
          hitReviewPolicy))
      hit = HIT(hitType,
                mTurkHIT.getHITId,
                prompt,
                mTurkHIT.getCreationTime.getTime.getTime)
      _ <- FileManager.saveHIT(hit)
    } yield hit
  }

  final def reviewHITs(implicit w: Writer[Response]): List[Assignment[Response]] = for {
    mTurkHIT <- service.searchAllHITs.filter(hit => hit.getHITTypeId == hitType).toList
    _ = disposeFinishedHITs
    assignment <- reviewHIT(mTurkHIT)
  } yield assignment

  final def reviewHIT(
    mTurkHIT: MTurkHIT
  )(implicit w: Writer[Response]): List[Assignment[Response]] = {
    // get submissions and review them
    val assignments = service.getAllAssignmentsForHIT(mTurkHIT.getHITId())
    val submittedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
    val approvedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Approved))

    // extend the HIT?
    // if(submittedAssignments.size + approvedAssignments.size < numAssignmentsPerHIT) {
    //   // If we don't have all of the assignments in, then extend the HIT.
    //   // We do this because the only reason it would be reviewable is if it had expired.
    //   service.extendHIT(mTurkHIT.getHITId, 1, lifetime)
    //   println
    //   println("Extended HIT: " + mTurkHIT.getHITId);
    //   println("You may see your HIT with HITTypeId '" + mTurkHIT.getHITTypeId + "' here: ");
    //   println(service.getWebsiteURL + "/mturk/preview?groupId=" + mTurkHIT.getHITTypeId);
    // }

    // review the submitted assignments and return the newly approved annotations in a list.
    for {
      assignment <- submittedAssignments.toList
      annotation <- reviewAssignment(mTurkHIT, assignment)
    } yield annotation
  }

  final def disposeFinishedHITs: Unit = {
    for (mTurkHIT <- service.getAllReviewableHITs(hitType)) {
      // check if the HIT is done and dispose of it if necessary.
      val assignments = service.getAllAssignmentsForHIT(mTurkHIT.getHITId)
      val submittedAssignments = assignments.filter(a =>
        a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
      val approvedAssignments = assignments.filter(a =>
        a.getAssignmentStatus.equals(AssignmentStatus.Approved))
      if(approvedAssignments.size >= numAssignmentsPerHIT && submittedAssignments.size == 0) {
        // If we have approved enough assignments, and no more are in the queue, dispose of the HIT.
        service.disposeHIT(mTurkHIT.getHITId())
        println
        println(s"Disposed HIT: ${mTurkHIT.getHITId()}")
        println(s"HIT type of disposed: $hitType")
      }
    }
  }

  // Contract: returns Some(Annotation) if and only if we approve the assignment.
  // Otherwise returns None.
  final def reviewAssignment(
    mTurkHIT: MTurkHIT,
    mTurkAssignment: MTurkAssignment
  )(implicit w: Writer[Response]): Option[Assignment[Response]] = {
    val assignment = Assignment(
      hitType = hitType,
      hitId = mTurkHIT.getHITId,
      assignmentId = mTurkAssignment.getAssignmentId,
      workerId = mTurkAssignment.getWorkerId,
      acceptTime = mTurkAssignment.getAcceptTime.getTime.getTime,
      submitTime = mTurkAssignment.getSubmitTime.getTime.getTime,
      response = extractResponse(mTurkAssignment.getAnswer),
      feedback = extractFeedback(mTurkAssignment.getAnswer))

    evaluateAssignment(assignment) match {
      case Approval(message) => FileManager.saveAssignment(assignment).toOptionPrinting.map { _ =>
        service.approveAssignment(assignment.assignmentId, message)
        println
        println(s"Approved assignment: ${assignment.assignmentId}")
        println(s"HIT for approved assignment: ${mTurkHIT.getHITId}")
        println(s"HIT type for approved assignment: $hitType")
        assignment
      }
      case Rejection(message) => FileManager.saveRejectedAssignment(assignment).toOptionPrinting.map { _ =>
        service.rejectAssignment(assignment.assignmentId, message)
        println
        println(s"Rejected assignment: ${assignment.assignmentId}")
        println(s"Reason: $message")
        println(s"HIT for rejected assignment: ${mTurkHIT.getHITId}")
        println(s"HIT type for rejected assignment: $hitType")
        assignment
      }
    }
  }

  def evaluateAssignment(a: Assignment[Response]): AssignmentEvaluation = Approval("")

  // blah, not really happy about this being here
  sealed trait Message
  object Message {
    case object Start extends Message
    case object Stop extends Message
    case object Update extends Message
    case object Expire extends Message
    case object Disable extends Message
    case class AddPrompt(p: Prompt) extends Message
  }
}
