package turkey
package tasks

import turkey.util._

import com.amazonaws.mturk.requester.{HIT => MTurkHIT}
import com.amazonaws.mturk.requester.{Assignment => MTurkAssignment}
import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.requester.ReviewPolicy
import com.amazonaws.mturk.requester.PolicyParameter
import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import java.util.Calendar

import scala.util.Try
import scala.concurrent.duration._

import akka.stream.scaladsl.Flow

import upickle.default._

/** Specifies a kind of task to run on MTurk.
  *
  * The code defining an individual task type will be here.
  * An instance of this class will correspond to a single HIT Type ID,
  * which is Mechanical Turk's way of categorizing HITs uploaded to the system.
  * This specifies the method to convert from `Prompt`s
  * (as in, the type parameter seen all over this project) into XML strings
  * that are POSTed to the MTurk API as questions shown to annotators.
  * It also has a method for converting from annotator responses (also XML strings)
  * into `Response`s.
  *
  * To implement the logic of an actual task, the work is done in the client-side code.
  *
  * NOTE: As of now, we've not used qualification requirements and the code assumes there are none.
  * If you wish to use qualification requirements it may require modifying the code; I don't know.
  *
  * @tparam Prompt
  * @tparam Response
  */
sealed trait TaskSpecification {
  type Prompt
  implicit val promptWriter: Writer[Prompt]
  type Response
  implicit val responseReader: Reader[Response]
  type ApiRequest
  implicit val apiRequestReader: Reader[ApiRequest]
  type ApiResponse
  implicit val apiResponseWriter: Writer[ApiResponse]
  implicit val config: TaskConfig

  val taskKey: String
  val hitType: HITType
  val apiFlow: Flow[ApiRequest, ApiResponse, Any]
  val samplePrompt: Prompt
  val frozenHITTypeId: Option[String]

  import hitType._
  import config._

  /** The HIT Type ID for this task.
    *
    * When this is accessed with a certain set of parameters for the first time,
    * a new HIT Type ID will be registered on Amazon's systems.
    * Subsequent calls with the same parameters will always return this same value,
    * for the life of the HIT Type (which I believe expires 30 days after the last time it is used.
    * It may be 90 days. TODO check on that. But it doesn't really matter...)
    *
    * I'm not 100% sure this needs to be lazy... but it's not hurting anyone as it is.
    */
  final lazy val hitTypeId = frozenHITTypeId.getOrElse(hitType.register(service))

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
  final def createHIT(
    prompt: Prompt,
    numAssignments: Int,
    lifetime: Long = 2592000L /* seconds (30 days) */): Try[HIT[Prompt]] = {

    val questionXML = createQuestionXML(prompt)

    // just hash the time and main stuff of our request for the unique token.
    val uniqueRequestToken = (hitTypeId, questionXML, System.nanoTime()).toString.hashCode.toString

    for {
      mTurkHIT <- Try(
        service.createHIT(
          hitTypeId,
          title,
          description,
          keywords,
          questionXML,
          reward,
          assignmentDuration,
          autoApprovalDelay,
          lifetime,
          numAssignments,
          "", // don't bother with annotation---we don't get it back and it causes errors if >255 bytes (which was documented NOWHERE)
          qualRequirements,
          Array("Minimal", "HITQuestion", "HITDetail"), // response groups --- these don't actually do anything :(
          uniqueRequestToken,
          assignmentReviewPolicy,
          hitReviewPolicy))
      hit = HIT(hitTypeId,
                mTurkHIT.getHITId,
                prompt,
                mTurkHIT.getCreationTime.getTime.getTime)
      _ <- hitDataService.saveHIT(hit)
    } yield hit
  }

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
  final def extractResponse(answerXML: String): Response =
    read[Response](getAnswers(answerXML)(responseLabel))

  /** Extracts the annotator's feedback from an answer XML string.
    *
    * The feedback field needs to be manually incorporated into the question's XML.
    * In theory, the feedback could be incorporated into the Response data type,
    * but since I always found myself wanting a feedback field anyway, this was vastly more convenient.
    * (If you don't implement feedback, it just returns the empty string.)
    * Notes from the documentation for `extractResponse` apply here.
    *
    * @param answerXML the XML string received from the API
    * @return the annotator's feedback
    */
  final def extractFeedback(answerXML: String): String =
    getAnswers(answerXML).get(feedbackLabel).getOrElse("")

  final def makeAssignment(hitId: String, mTurkAssignment: MTurkAssignment): Assignment[Response] =
    Assignment(
      hitTypeId = hitTypeId,
      hitId = hitId,
      assignmentId = mTurkAssignment.getAssignmentId,
      workerId = mTurkAssignment.getWorkerId,
      acceptTime = mTurkAssignment.getAcceptTime.getTime.getTime,
      submitTime = mTurkAssignment.getSubmitTime.getTime.getTime,
      response = extractResponse(mTurkAssignment.getAnswer),
      feedback = extractFeedback(mTurkAssignment.getAnswer))

  // == Private methods and fields ==

  // auxiliary method for extracting response and feedback
  private[this] final def getAnswers(answerXML: String) = {
    import scala.collection.JavaConverters._
    RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
      .toMap
  }

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
  private[this] final def createQuestionXML(prompt: Prompt): String = {
    s"""
      <?xml version="1.0" encoding="UTF-8"?>
      <HTMLQuestion xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd">
        <HTMLContent><![CDATA[
          <!DOCTYPE html>${TaskPage.htmlPage(prompt, this).render}
        ]]></HTMLContent>
        <FrameHeight>600</FrameHeight>
      </HTMLQuestion>
    """.trim
  }
}
object TaskSpecification {
  private[this] case class TaskSpecificationImpl[P, R, ApiReq, ApiResp](
    override val taskKey: String,
    override val hitType: HITType,
    override val apiFlow: Flow[ApiReq, ApiResp, Any],
    override val samplePrompt: P,
    override val frozenHITTypeId: Option[String])(
    implicit override val promptWriter: Writer[P],
    val responseReader: Reader[R],
    val apiRequestReader: Reader[ApiReq],
    val apiResponseWriter: Writer[ApiResp],
    val config: TaskConfig) extends TaskSpecification {

    override type Prompt = P
    override type Response = R
    override type ApiRequest = ApiReq
    override type ApiResponse = ApiResp
  }
  def apply[P, R, ApiReq, ApiResp](
    taskKey: String,
    hitType: HITType,
    apiFlow: Flow[ApiReq, ApiResp, Any],
    samplePrompt: P,
    frozenHITTypeId: Option[String] = None)(
    implicit promptWriter: Writer[P],
    responseReader: Reader[R],
    apiRequestReader: Reader[ApiReq],
    apiResponseWriter: Writer[ApiResp],
    config: TaskConfig): TaskSpecification { type Prompt = P; type Response = R; type ApiRequest = ApiReq; type ApiResponse = ApiResp } =
    TaskSpecificationImpl[P, R, ApiReq, ApiResp](taskKey, hitType, apiFlow, samplePrompt, frozenHITTypeId)
}
