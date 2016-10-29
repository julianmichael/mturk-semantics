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

/** Specifies a kind of task to run on MTurk.
  *
  * The code defining what annotators see and do will be here.
  * An instance of this class will correspond to a single HIT Type ID,
  * which is Mechanical Turk's way of categorizing HITs uploaded to the system.
  * This specifies the method to convert from "Prompts"
  * (as in, the type parameter seen all over this project) into XML strings
  * that are POSTed to the MTurk API as questions shown to annotators.
  * It also has a method for converting from annotator responses (also XML strings)
  * into "Responses."
  *
  * HIT Types:
  * Any operations involving HITs for a task will be applied to all HITs
  * that have the tasks's HIT Type ID.
  * This gives us a way to segregate our interactions with the API between different tasks,
  * allowing for running two different kinds of tasks in parallel
  * (for example, a Q/A gathering task and validation task).
  * The HIT Type ID of a HIT is determined by a set of values:
  * its title, description, keyword, rewards, auto-approval delay, assignment duration,
  * qualification requirements, assignment review policy, and HIT review policy.
  * We don't bother with assignment review policies or HIT review policies,
  * which are MTurk's way of automating assignment review; instead, just review them
  * here in the code.
  * The rest of these fields are left for an implementation to specify.
  * If you wish tasks to be segregated / run separately, make sure at least one of these fields
  * differs between them.
  *
  * NOTE: As of now, we've not used qualification requirements and the code assumes there are none.
  * If you wish to use qualification requirements it may require modifying the code; I don't know.
  *
  * @tparam Prompt
  * @tparam Response
  */
trait TaskSpecification[Prompt, Response] {
  import Config._

  // == HIT type fields ==

  /** The title of the HIT type; one of the first things workers see when browsing. */
  val title: String

  /** A longer (but still short) description of the task, displayed when a worker clicks on the HIT group while browsing. */
  val description: String

  /** Comma-separated keywords, helping workers find the task through search. */
  val keywords: String

  /** The payment, in dollars, to a worker for having an assignment approved. */
  val reward: Double

  /** The time, in seconds, before MTurk will automatically approve an assignment if you do not approve/reject it manually. */
  val autoApprovalDelay = 3600L // seconds (1 hour)

  /** The time, in seconds, allowed for a worker to complete a single assignment. */
  val assignmentDuration = 600L // seconds (10 minutes)

  /** The qualification requirements a worker must satisfy to work on the task. */
  val qualRequirements = Array.empty[QualificationRequirement]

  // == other fields ==

  /** The number of workers we solicit a response from for each HIT. */
  val numAssignmentsPerHIT: Int

  /** The amount of time, in seconds, that a HIT remains available to workers before expiring.
    * After this period, it may still be manually extended.
    */
  val lifetime = 2592000L // seconds (30 days)

  // == Q/A specification methods ==

  /** Creates the "question" XML object to send to the MTurk API when creating a HIT.
    *
    * See http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionAnswerDataArticle.html
    * for a specification of the XML documents that may be sent to the API as questions.
    * The result should include the full text giving instructions on how to do the task,
    * whereas the Prompt object should contain only the information necessary for a single specific question.
    *
    * @param prompt the well-typed data representation of a question
    * @return the MTurk-ready XML representation of a question
    */
  def createQuestionXML(prompt: Prompt): String

  /** Extracts the annotator's response from an "answer" XML object retrieved from the MTurk API
    * after the completion of an assignment.
    *
    * See http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionAnswerDataArticle.html
    * for a specification of the XML documents that may be received from the API as answers.
    * There are helpful classes in the Java API for parsing this XML; see implementations of this method
    * for examples.
    *
    * @param answerXML the XML string received from the API
    * @return the well-typed data representation of an annotator response
    */
  def extractResponse(answerXML: String): Response

  /** Extracts the annotator's feedback from an answer XML string.
    *
    * The feedback field needs to be manually incorporated into the question's XML.
    * In theory, the feedback could be incorporated into the Response data type,
    * but since I always found myself wanting a feedback field anyway, this was vastly more convenient.
    * (If you wish to skip it, just always return the empty string from this method.)
    * Notes from the documentation for `extractResponse` apply here.
    *
    * @param answerXML the XML string received from the API
    * @return the annotator's feedback
    */
  def extractFeedback(answerXML: String): String

  /** Decides whether to approve or reject an assignment and provides an explanation.
    *
    * Assumes the assignment is NOT saved to disk, and DOES NOT save the assignment to disk.
    * (Partly this is because where it is eventually saved is determined by whether it is approved.)
    *
    * @param assignment the completed assignment
    */
  def evaluateAssignment(assignment: Assignment[Response]): AssignmentEvaluation =
    Approval("")

  // == Final members and methods ==

  /** Importable container for the Message ADT. */
  final object Message {
    /** Type of messages that may be sent to a TaskManager for this task. */
    sealed trait Message
    case object Start extends Message
    case object Stop extends Message
    case object Update extends Message
    case object Expire extends Message
    case object Disable extends Message
    case class AddPrompt(p: Prompt) extends Message
  }

  // really not gonna bother with Amazon's automatic review policies for anything
  private[this] final val assignmentReviewPolicy = null
  private[this] final val hitReviewPolicy = null

  /** The HIT Type ID for this task.
    *
    * When this is accessed with a certain set of parameters for the first time,
    * a new HIT Type ID will be registered on Amazon's systems.
    * Subsequent calls with the same parameters will always return this same value,
    * for the life of the HIT Type (which I believe expires 30 days after the last time it is used.
    * It may be 90 days. TODO check on that. But it doesn't really matter...)
    *
    * I'm not 100% sure this needs to be lazy but since it makes a server call I thought it probably should be.
    */
  final lazy val hitType = service.registerHITType(
    autoApprovalDelay,
    assignmentDuration,
    reward,
    title,
    keywords,
    description,
    qualRequirements)

  /** Creates a HIT on MTurk.
    *
    * If the HIT is successfully created, saves the HIT to disk and returns it.
    * Otherwise returns a Failure with the error.
    *
    * Saving the HIT requires a serializer for it; for this reason,
    * the method needs a upickle serializer for the Prompt type.
    *
    * @param prompt the data from which to generate the question for the HIT
    * @param w a upickle serializer for Prompt
    * @return the created HIT, wrapped in a Try in case of error
    */
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

  /** Reviews submissions to all HITs of this task's HIT type and return the approved results.
    *
    * This method triggers reviewing of all of the assignments that have been submitted
    * for HITs of this task's HIT type, including sending approvals/rejections to the API.
    * The returned value consists only of the assignments that were approved.
    *
    * @param w a upickle serializer for Response
    * @return any newly approved assignments for this task
    */
  final def reviewHITs(implicit w: Writer[Response]): List[Assignment[Response]] = for {
    mTurkHIT <- service.searchAllHITs.filter(hit => hit.getHITTypeId == hitType).toList
    assignment <- reviewHIT(mTurkHIT)
    _ = disposeFinishedHITs
  } yield assignment

  // == Private helper methods ==

  private[this] final def reviewHIT(
    mTurkHIT: MTurkHIT
  )(implicit w: Writer[Response]): List[Assignment[Response]] = {
    // get submissions and review them
    val assignments = service.getAllAssignmentsForHIT(mTurkHIT.getHITId())
    val submittedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
    val approvedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Approved))

    // TODO if the HIT has expired, we should extend it. (low priority)
    // how do we extend its lifetime without adding more max annotators?
    // the API complained when I tried to pass in 0...
    // if(/* the HIT has expired */) {
    //   service.extendHIT(mTurkHIT.getHITId, 1, lifetime)
    //   println
    //   println("Extended HIT: " + mTurkHIT.getHITId);
    //   println(service.getWebsiteURL + "/mturk/preview?groupId=" + mTurkHIT.getHITTypeId);
    // }

    // review the submitted assignments and return the newly approved annotations in a list.
    for {
      assignment <- submittedAssignments.toList
      annotation <- reviewAssignment(mTurkHIT, assignment)
    } yield annotation
  }

  private[this] final def disposeFinishedHITs: Unit = {
    // "Reviewable" HITs will have all of their assignments Submitted, Approved, or Rejected; or, they will be expired.
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
        println(s"Disposed HIT: ${mTurkHIT.getHITId()}; $hitType")
      }
    }
  }

  /** Approves/rejects the assignment only if we successfully save it to disk.
    * Returns Some(assignment) iff we approve the assignment.
    * Returns None if either we rejected the assignment or we did not succeed in approving/rejecting it.
    */
  private[this] final def reviewAssignment(
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
        println(s"HIT for approved assignment: ${mTurkHIT.getHITId}; $hitType")
        assignment
      }
      // TODO when we reject one, extend the HIT with another assignment.
      case Rejection(message) => FileManager.saveRejectedAssignment(assignment).toOptionPrinting.map { _ =>
        service.rejectAssignment(assignment.assignmentId, message)
        println
        println(s"Rejected assignment: ${assignment.assignmentId}")
        println(s"HIT for rejected assignment: ${mTurkHIT.getHITId}; $hitType")
        println(s"Reason: $message")
        assignment
      }
    }
  }
}
