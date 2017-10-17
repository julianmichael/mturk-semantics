package turksem.iqa

import cats.implicits._

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr.Pring
import turksem.qamr.SaveData

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

import AdaptiveQuestionGuesser.ops._

class IQAHITManager[SID : Reader : Writer : HasTokens](
  helper: HITManager.Helper[IQAPrompt[SID], IQAResponse],
  initQuestionGuesser: CountBasedQuestionGuesser,
  numAssignmentsForPrompt: IQAPrompt[SID] => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[IQAPrompt[SID]])(
  implicit annotationDataService: AnnotationDataService,
  inflections: Inflections
) extends NumAssignmentsHITManager[IQAPrompt[SID], IQAResponse](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource
) with StrictLogging {

  import helper._
  import config._
  import taskSpec.hitTypeId

  val questionGuesserFilename = "questionGuesser"

  var questionGuesser = annotationDataService.loadLiveData(questionGuesserFilename)
    .map(_.mkString)
    .map(read[CountBasedQuestionGuesser])
    .toOption
    .getOrElse {
    logger.warn("Could not find save file for question guesser; starting anew")
    initQuestionGuesser
  }

  private[this] def save = {
    annotationDataService.saveLiveData(
      questionGuesserFilename,
      write(questionGuesser))
    logger.info("Question guesser data saved.")
  }

  override def reviewAssignment(hit: HIT[IQAPrompt[SID]], assignment: Assignment[IQAResponse]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    val tokens = hit.prompt.id.tokens
    val inflectedTokens = PosTagger.posTag(tokens).map(w =>
      InflectionalWord(
        token = w.token,
        pos = w.pos,
        index = w.index,
        inflectedFormsOpt = inflections.getInflectedForms(tokens(w.index).lowerCase))
    )
    questionGuesser = questionGuesser.update(inflectedTokens, assignment.response.qas)

  }
}
