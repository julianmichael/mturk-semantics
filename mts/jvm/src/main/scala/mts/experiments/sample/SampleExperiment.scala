package mts.experiments.sample

import mts.experiments._
import mts.core._
import mts.util._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language.tokenize

import akka.actor._

import scala.concurrent.duration._
import scala.language.postfixOps

class SampleExperiment(implicit config: TaskConfig) {
  val experimentName = "sample"

  // val hitsToKeepActive = if(config.isProduction) 25 else 3
  // val numAssignmentsPerHIT = if(config.isProduction) 4 else 1

  val sampleHITType = HITType(
    title = s"Sample task: is this sentence good?",
    description = s"""
      Given a sentence, indicate whether it is good.
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  lazy val taskSpec = TaskSpecification[SamplePrompt, SampleResponse](sampleHITType)
  lazy val hitManager = new SampleHITManager(taskSpec)

  lazy val system = ActorSystem("system")
  lazy val server = new SampleServer
  lazy val actor = system.actorOf(Props(TaskManager(hitManager)))

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
