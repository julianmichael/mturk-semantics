package mts.experiments

import mts.core._
import mts.util._
import mts.tasks._
import mts.conll._
import mts.qa._

import akka.actor._

import scala.util.Try
import scala.util.Random

// file name has B_ prepended so experiments have an obvious order
object ValidationExperiment {
  val experimentName = "b_validation"

  type QuestionData = (CoNLLSentencePath, List[String]) // path to sentence, original QA pairs
  type AnswerData = (List[ValidationAnswer], String) // answers (+ invalid question flag), feedback

  lazy val questionData: List[QuestionData] = {
    val rand = new Random(42) // DO NOT CHANGE THE SEED
    // it has to be consistent so that after restarts the set of questions will be the same.
    val dataPairs = for {
      ((path, _), responses) <- OpenFormExperiment.getAllQAPairs()
      (qaPairs, _) <- responses
      (q, a) <- qaPairs
    } yield (path, q)
    val dataItems = dataPairs.groupBy(_._1).flatMap {
      case (path, pairs) => rand.shuffle(pairs).map(_._2).grouped(3).map(g => (path, g.toList))
    }
    rand.shuffle(dataItems).toList
  }

  lazy val taskSpec = ValidationTask(numAssignmentsPerHIT = 3)

  lazy val system = ActorSystem("system")

  lazy val actor = taskSpec.createMonitor(system, questionData.iterator, 200)

  def start() = actor ! taskSpec.Message.Start
  def stop() = actor ! taskSpec.Message.Stop
  def disable() = actor ! taskSpec.Message.Disable
  def expire() = actor ! taskSpec.Message.Expire
  def update() = actor ! taskSpec.Message.Update

  // TODO save HIT types and access them here and such
  def getAllAnnotations(): List[Annotation] =
    FileManager.loadAnnotationsForHITType(taskSpec.hitType)

  def getAllQAPairs(): Iterable[(QuestionData, List[AnswerData])] =
    taskSpec.annotatedQAPairs.toMap.values

  def getAllFeedback(): Iterable[String] = getAllQAPairs()
    .flatMap(_._2.map(_._2))
    .filterNot(_.isEmpty)
}
