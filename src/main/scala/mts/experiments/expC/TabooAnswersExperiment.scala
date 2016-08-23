package mts.experiments.expC

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

object TabooAnswersExperiment {
  // file names should have b_ prepended so experiments have an obvious order
  val experimentName = "c_taboo-answers"

  // lazy val prompts: List[ValidationPrompt] = {
  //   val rand = new Random(42) // DO NOT CHANGE THE SEED
  //   // it has to be consistent so that after restarts the set of questions will be the same.
  //   val validationQuestions = for {
  //     annotation <- OpenFormExperiment.getAllAnnotations()
  //     (path, _) = OpenFormExperiment.protoTaskSpec.extractPrompt(annotation.question.get)
  //     (qaPairs, _) = OpenFormExperiment.protoTaskSpec.extractResponse(annotation)
  //     (q, a) <- qaPairs
  //   } yield ValidationQuestion(path, annotation.workerId, q, a)
  //   val validationPrompts = validationQuestions.groupBy(_.path).toList.flatMap {
  //     case (path, vQuestions) => rand.shuffle(vQuestions).grouped(3).map(g => ValidationPrompt(path, g.toList))
  //   }
  //   rand.shuffle(validationPrompts).toList
  // }

  lazy val taskSpec = TabooAnswersTask

  // need to decide how to initialize the data..
  lazy val dataManager = new TabooAnswersDataManager(taskSpec.hitType, ???)

  lazy val taskManager = TaskManager(taskSpec, dataManager, numHITsToKeepActive = 5)

  lazy val system = ActorSystem("system")

  lazy val actor = system.actorOf(Props(taskManager))

  def start() = actor ! taskManager.Message.Start
  def stop() = actor ! taskManager.Message.Stop
  def disable() = actor ! taskManager.Message.Disable
  def expire() = actor ! taskManager.Message.Expire
  def update() = actor ! taskManager.Message.Update
}
