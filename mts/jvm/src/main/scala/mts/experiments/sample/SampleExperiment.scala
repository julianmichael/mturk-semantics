package mts.experiments.sample

import mts.experiments._
import mts.core._
import mts.util._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language.tokenize

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

class SampleExperiment(implicit config: TaskConfig) {
  val experimentName = "sample"

  // for now, must agree with a string specified on the client as well. TODO refactor this
  val sampleHITType = HITType(
    title = s"Sample task: is this sentence good?",
    description = s"""
      Given a sentence, indicate whether it is good.
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  lazy val sampleApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(path, FileManager.getCoNLLSentence(path).get)
  }

  lazy val taskSpec = TaskSpecification[SamplePrompt, SampleResponse, ApiRequest, ApiResponse](
    TaskIndex.sampleTaskKey, sampleHITType, sampleApiFlow, SamplePrompt(sentences.head._1))
  lazy val hitManager = new SampleHITManager[SamplePrompt, SampleResponse](taskSpec)

  import config.actorSystem
  lazy val server = new Server(List(taskSpec))
  lazy val actor = actorSystem.actorOf(Props(TaskManager(hitManager)))

  import hitManager.Message._
  def start(interval: FiniteDuration = 1 minute) = {
    server
    actor ! Start(interval)
  }
  def stop() = actor ! Stop
  def disable() = actor ! Disable
  def expire() = actor ! Expire
  def update() = {
    server
    actor ! Update
  }
}
