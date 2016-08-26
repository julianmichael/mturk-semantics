package mts.experiments.expC

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.util._
import mts.tasks._
import mts.conll._
import mts.language.tokenize

import akka.actor._

import scala.util.Try
import scala.util.Random

import scala.concurrent.duration._
import scala.language.postfixOps

object TabooAnswersExperiment {
  // file names should have b_ prepended so experiments have an obvious order
  val experimentName = "c_taboo-answers"

  val sentencePaths = {
    val allSentences = for {
      path <- mts.experiments.annotationFilepaths.iterator
      file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
      sentence <- file.sentences
      if sentence.sentenceNum % 2 == 0 || sentence.sentenceNum % 5 == 0 // skip some of the sentences
      if sentence.words.size > 6 // don't do the super short sentences
    } yield CoNLLSentencePath(path, sentence.sentenceNum)
    allSentences.take(40).toList
  }

  val hitsToKeepActive = if(Config.isProduction) 25 else 3

  lazy val taskSpec = TabooAnswersTask

  // need to decide how to initialize the data..
  lazy val dataManager = new TabooAnswersDataManager(
    taskSpec.hitType,
    sentencePaths.iterator,
    FileManager.loadAllData(taskSpec.hitType))

  lazy val system = ActorSystem("system")

  lazy val actor = system.actorOf(Props(TaskManager(
                                          taskSpec,
                                          dataManager,
                                          numHITsToKeepActive = hitsToKeepActive)))

  def start() = actor ! taskSpec.Message.Start
  def stop() = actor ! taskSpec.Message.Stop
  def disable() = actor ! taskSpec.Message.Disable
  def expire() = actor ! taskSpec.Message.Expire
  def update() = actor ! taskSpec.Message.Update

  def readableQATSV(): String = {
    val data = FileManager.loadAllData[TabooAnswersPrompt, TabooAnswersResponse](taskSpec.hitType)
    val dataBySentence = data.groupBy(_._1.prompt.path)
    def forSentence(path: CoNLLSentencePath) = for {
      (hit, assignment :: Nil) <- dataBySentence(path).sortBy(_._1.creationTime)
      TabooAnswersResponse(qaPairs) = assignment.response
      (q, a) <- qaPairs
      TabooAnswersPrompt(_, tabooList) = hit.prompt
    } yield {
      val workerId = assignment.workerId
      val feedback = assignment.feedback
      s"$workerId\t$q\t$a\t$feedback\t${tabooList.mkString("\t")}"
    }
    val result = for {
      path <- dataBySentence.keys
    } yield {
      val sentenceString = TextRendering.renderSentence(FileManager.getCoNLLSentence(path).get)
      s"$sentenceString\n" + forSentence(path).mkString("\n")
    }
    result.mkString("\n\n")
  }
}
