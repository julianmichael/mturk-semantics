package turksem.gapfill

import cats.implicits._

import spacro._
import spacro.tasks._

import turksem._
import turksem.util._
import qamr.Pring
import qamr.SaveData
import qamr.AnnotationDataService

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

import QuestionGuesserProducer.ops._

class GapfillHITManager[
  SID : Reader : Writer : HasTokens,
  GuesserProducer : Reader : Writer](
  helper: HITManager.Helper[GapfillPrompt[SID], GapfillResponse],
  initGuesserProducer: GuesserProducer,
  numAssignmentsForPrompt: GapfillPrompt[SID] => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[GapfillPrompt[SID]])(
  implicit annotationDataService: AnnotationDataService,
  inflections: Inflections,
  adaptiveGuesserInstance: QuestionGuesserProducer[GuesserProducer] { type SentenceId = SID }
) extends NumAssignmentsHITManager[GapfillPrompt[SID], GapfillResponse](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource
) with StrictLogging {

  import helper._
  import config._
  import taskSpec.hitTypeId

  val guesserProducerFilename = "guesserProducer"

  var guesserProducer = annotationDataService.loadLiveData(guesserProducerFilename)
    .map(_.mkString)
    .map(read[GuesserProducer])
    .toOption
    .getOrElse {
    logger.warn("Could not find save file for question guesser producer; starting anew")
    initGuesserProducer
  }

  private[this] def save = {
    annotationDataService.saveLiveData(
      guesserProducerFilename,
      write(guesserProducer))
    logger.info("Question guesser data saved.")
  }

  override def reviewAssignment(hit: HIT[GapfillPrompt[SID]], assignment: Assignment[GapfillResponse]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    guesserProducer = guesserProducer.update(hit.prompt.id, assignment.response.qas)
  }
}
