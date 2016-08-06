package mts.experiments

import mts._
import mts.tasks._
import mts.conll._
import akka.actor._

// file name has A_ prepended so experiments have an obvious order
object OpenFormExperiment {
  // get 100 sentences from conll data
  val sentences: List[(CoNLLSentencePath, CoNLLSentence)] = ???

  // bucket sentences and create a task for each bucket
  def makeTask(minTokens: Int, maxTokens: Int, numQAs: Int, reward: Double) = ???

  val tasks = List.empty[(MTurkOpenFormHTMLQA, ActorRef)]

  def start() = tasks.foreach(p => p._2 ! p._1.Message.Start)
  def stop() = tasks.foreach(p => p._2 ! p._1.Message.Stop)
  def disable() = tasks.foreach(p => p._2 ! p._1.Message.Disable)
  def expire() = tasks.foreach(p => p._2 ! p._1.Message.Expire)
  def update() = tasks.foreach(p => p._2 ! p._1.Message.Update)

}
