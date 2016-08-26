package mts.experiments.expD

import mts.analysis._
import mts.experiments._
import mts.experiments.expA._
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

object WordChoosingExperiment {
  // file names should have b_ prepended so experiments have an obvious order
  val experimentName = "d_word-choosing"

  val sentencePaths = {
    val allSentences = for {
      path <- mts.experiments.expA.OpenFormExperiment.annotationFilepaths.iterator
      file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
      sentence <- file.sentences
      if sentence.sentenceNum % 2 == 0 || sentence.sentenceNum % 5 == 0 // skip some of the sentences
      if sentence.words.size > 6 // don't do the super short sentences
    } yield CoNLLSentencePath(path, sentence.sentenceNum)
    allSentences.take(100).toList
  }

  // why not keep all HITs active at once?
  // I think this way we get more annotators per HIT, faster. This reduces the latency between
  // when the first worker submits an assignment and when that assignment is reviewed
  // (right now, that only happens after all assignments are in).
  // When traffic is low this is helpful for getting the data saved quicker to easily see what is going on.
  // It also is good to review the HITs and pay the workers (including bonuses) as quickly as possible.
  // NOTE: might be a good idea to increase this halfway through once things get going...
  val hitsToKeepActive = if(Config.isProduction) 25 else 3
  val numAssignmentsPerHIT = if(Config.isProduction) 4 else 1

  lazy val taskSpec = new WordChoosingTask(numAssignmentsPerHIT)

  lazy val dataManager = new WordChoosingDataManager(
    taskSpec.hitType,
    numAssignmentsPerHIT,
    taskSpec.bonus,
    sentencePaths.iterator.map(WordChoosingPrompt.apply),
    FileManager.loadAllData[WordChoosingPrompt, WordChoosingResponse](taskSpec.hitType))

  lazy val system = ActorSystem("system")

  lazy val actor = system.actorOf(Props(TaskManager(
                                          taskSpec,
                                          dataManager,
                                          numHITsToKeepActive = hitsToKeepActive,
                                          interval = 1 minute)))

  def start() = actor ! taskSpec.Message.Start
  def stop() = actor ! taskSpec.Message.Stop
  def disable() = actor ! taskSpec.Message.Disable
  def expire() = actor ! taskSpec.Message.Expire
  def update() = actor ! taskSpec.Message.Update

  def readableQATSV(): String = {
    val data = FileManager.loadAllData[WordChoosingPrompt, WordChoosingResponse](taskSpec.hitType)
    def forAssignments(assignments: List[Assignment[WordChoosingResponse]]) = for {
      assignment <- assignments.sortBy(_.submitTime)
      WordChoosingResponse(items) = assignment.response
      WordChoosingResponseItem(qWord, aWord, q, a) <- items
    } yield {
      val workerId = assignment.workerId
      val feedback = assignment.feedback
      s"$workerId\t$qWord\t$aWord\t$q\t$a\t$feedback"
    }
    val result = for {
      (hit, assignments) <- data.sortBy(_._1.creationTime)
    } yield {
      val sentenceString = TextRendering.renderSentence(FileManager.getCoNLLSentence(hit.prompt.path).get)
      s"$sentenceString\n" + forAssignments(assignments).mkString("\n")
    }
    result.mkString("\n\n")
  }
}
