package mts.experiments.expE

import mts.experiments._
import mts.core._
import mts.util._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

class QuestionWordExperiment(implicit config: TaskConfig) {
  val experimentName = "questionWordQA"

  // for now, must agree with a string specified on the client as well. TODO refactor this
  val qaGenHITType = HITType(
    title = s"Write questions and answers about a word in context",
    description = s"""
      Given a sentence and a word from that sentence,
      write a question using the word whose answer is taken from the sentence.
      Come up with more question-answer pairs for a bonus!
    """.trim,
    reward = 0.05,
    keywords = "language,english,question answering")

  lazy val qaGenApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(path, FileManager.getCoNLLSentence(path).get)
  }

  val sampleQAGenPrompt = QAGenPrompt(sentences.head._1, 0)

  lazy val qaGenTaskSpec = TaskSpecification[QAGenPrompt, QAGenResponse, ApiRequest, ApiResponse](
    TaskIndex.expEQAGenTaskKey, qaGenHITType, qaGenApiFlow, sampleQAGenPrompt)

  lazy val sourceSentences = {
    val inOrder = for {
      (path, sentence) <- sentences.iterator
      i <- (0 until sentence.words.size)
      if !uninterestingTokens.contains(sentence.words(i).token)
    } yield QAGenPrompt(path, i)

    val shuffleRand = new util.Random(987654321L)

    shuffleRand.shuffle(inOrder.toVector).iterator
  }


  lazy val finishedOrActivePrompts = FileManager.loadAllData[QAGenPrompt, QAGenResponse](qaGenTaskSpec.hitTypeId)
    .map(_._1.prompt).iterator

  lazy val qaGenHITManager = new PromptOnceHITManager[QAGenPrompt, QAGenResponse](
    qaGenTaskSpec, 1, 3, sourceSentences, finishedOrActivePrompts)

  import config.actorSystem
  lazy val server = new Server(List(qaGenTaskSpec))
  lazy val qaGenActor = actorSystem.actorOf(Props(TaskManager(qaGenHITManager)))

  import qaGenHITManager.Message._
  def start(interval: FiniteDuration = 1 minute) = {
    server
    qaGenActor ! Start(interval)
  }
  def stop() = qaGenActor ! Stop
  def disable() = qaGenActor ! Disable
  def expire() = qaGenActor ! Expire
  def update() = {
    server
    qaGenActor ! Update
  }
}
