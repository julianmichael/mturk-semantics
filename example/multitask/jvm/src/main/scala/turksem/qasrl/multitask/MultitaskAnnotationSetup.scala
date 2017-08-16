package turksem.qasrl.multitask

import turksem.qamr._
import turksem.qamr.annotation._
import turksem.qasrl.annotation._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.structure._
import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Try

import upickle.default._

class MultitaskAnnotationSetup(implicit config: TaskConfig) {

  def numGenerationAssignmentsForPrompt(p: GenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    allIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService, isStopword,
    qualTest = MultitaskQualTest)
}
