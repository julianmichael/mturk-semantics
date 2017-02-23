package mts.experiments.expG

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

class WordLimitingQValidationExperiment(implicit config: TaskConfig) {
  val experimentName = "g_wordlimiting_qvalidation"

  val hitType = HITType(
    title = s"Simplify questions about a sentence",
    description = s"""
      You'll be given a sentence and a list of questions (and their answers).
      You will revise the questions picking a key word and removing/replacing as many other words as possible.
    """.trim,
    reward = 0.30,
    keywords = "language,english,question answering")

  lazy val sentenceApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) =>
      val sentence = FileManager.getCoNLLSentence(path).get
      val alignedTokens = for {
        w <- sentence.words
        t <- inflections.getAllForms(w.token.lowerCase)
        if !reallyUninterestingTokens.contains(t)
      } yield (t.toString, w.index)
      SentenceResponse(sentence, alignedTokens.toSet)
  }

  import expE.{QuestionWordExperiment, QAGenPrompt, QAGenResponse}
  lazy val experimentE = new QuestionWordExperiment
  lazy val expEHITInfos = experimentE.loadQAGenData.map(GenIso.fields[HITInfo[QAGenPrompt, QAGenResponse]].reverseGet)

  lazy val questionInfos = for {
    HITInfo(hit, assignments) <- expEHITInfos
    assignment <- assignments
    ((question, answer), index) <- assignment.response.qaPairs.zipWithIndex
    if !question.isEmpty && !answer.isEmpty
  } yield (hit.prompt.path, SourcedTokenizedQAPair(hit.hitId, index, tokenize(question).toVector, answer))

  val samplePrompt = TokenizedValidationPrompt(questionInfos(10)._1, questionInfos.filter(_._1 == questionInfos(10)._1).take(4).map(_._2))

  lazy val taskSpec = TaskSpecification[TokenizedValidationPrompt, KeywordQuestionValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expGWordLimQValTaskKey, hitType, sentenceApiFlow, samplePrompt)

  import config.actorSystem
  lazy val server = new Server(List(taskSpec))

  // lazy val helper = new HITManager.Helper(taskSpec)
  // lazy val hitManager = actorSystem.actorOf(Props(new NumAssignmentsHITManager(
  //   helper,
  //   numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
  //   numHITsToKeepActive = (if(config.isProduction) 30 else 3),
  //   prompts.iterator)))
  // lazy val actor = actorSystem.actorOf(Props(new TaskManager(helper, hitManager)))

  // import TaskManager._
  // def start(interval: FiniteDuration = 1 minute) = {
  //   server
  //   qvActor ! Start(interval)
  //   avActor ! Start(interval)
  //   lavActor ! Start(interval)
  // }
  // def stop() = {
  //   qvActor ! Stop
  //   avActor ! Stop
  //   lavActor ! Stop
  // }
  // def disable() = {
  //   qvActor ! Disable
  //   avActor ! Disable
  //   lavActor ! Disable
  // }
  // def expire() = {
  //   qvActor ! Expire
  //   avActor ! Expire
  //   lavActor ! Expire
  // }
  // def update() = {
  //   server
  //   qvActor ! Update
  //   avActor ! Update
  //   lavActor ! Update
  // }

}
