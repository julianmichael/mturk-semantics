package mts.experiments.sample

import mts.experiments._
import mts.core._
import mts.util._
import mts.tasks._
import mts.tasks._

import nlpdata.datasets.conll._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

class SampleExperiment(implicit config: TaskConfig) {
  val experimentName = "sample"

  val sampleHITType = HITType(
    title = s"Sample task: is this sentence good?",
    description = s"""
      Given a sentence, indicate whether it is good.
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  val sentences = List(
    "Hello, this is a sentence.",
    "This is another sentence.")

  lazy val sampleApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(id) => SentenceResponse(id, sentences(id))
  }

  val samplePrompt = SamplePrompt(0)

  lazy val taskSpec = TaskSpecification[SamplePrompt, SampleResponse, ApiRequest, ApiResponse](
    TaskIndex.sampleTaskKey, sampleHITType, sampleApiFlow, samplePrompt)
  lazy val helper = new HITManager.Helper(taskSpec)

  import config.actorSystem
  lazy val hitManager = actorSystem.actorOf(
    Props(NumAssignmentsHITManager.constAssignments[SamplePrompt, SampleResponse](
            helper, 1, 3, List(samplePrompt).iterator)))

  lazy val server = new Server(List(taskSpec))
  lazy val actor = actorSystem.actorOf(Props(new TaskManager(helper, hitManager)))

  import TaskManager._
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
