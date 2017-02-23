package mts.experiments.expG

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

class ManualQA(implicit config: TaskConfig) {
  val experimentName = "g_manualQA"

  val hitType = HITType(
    title = s"Manual interface for QA pairs",
    description = s"""
      For convenient interface to annotate gold data.
    """.trim,
    reward = 1.00,
    keywords = "language,english,question answering")

  import expE.{QuestionWordExperiment, QAGenPrompt, QAGenResponse}
  lazy val experimentE = new QuestionWordExperiment

  def loadSavedData = FileManager.loadDataFile(experimentName, "goldData.txt")
    .map(_.mkString("\n"))
    .map(read[List[SavedManualQARecord]]) match {
    case Failure(_) =>
      val newGold = experimentE.sourceSentences.map(Function.tupled(SavedManualQARecord.blank)).toList
      FileManager.saveDataFile(experimentName, "goldData.txt", write(newGold))
      newGold.map(_.load)
    case Success(x) => x.map(_.load)
  }

  var goldData = loadSavedData

  def convertToSaved(record: ManualQARecord) = record match {
    case ManualQARecord(path, _, groups) => SavedManualQARecord(path, groups)
  }

  lazy val apiFlow = Flow[ManualQAApiRequest].map {
    case AllRecordsRequest => Some(AllRecordsResponse(goldData))
    case AllRecordsUpdate(records) =>
      goldData = records
      FileManager.saveDataFile(experimentName, "goldData.txt", write(goldData.map(convertToSaved)))
      None
  }.collect {
    case Some(response) => response: ManualQAApiResponse
  }

  val samplePrompt = ()

  lazy val taskSpec = TaskSpecification[Unit, Unit, ManualQAApiRequest, ManualQAApiResponse](
    TaskIndex.expGManualQATaskKey, hitType, apiFlow, samplePrompt)

  import config.actorSystem
  lazy val server = new Server(List(taskSpec))

}
