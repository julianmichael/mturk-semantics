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
    reward = 0.20,
    assignmentDuration = 180L, // 3 minutes
    keywords = "language,english,question answering")

  lazy val sentenceApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) =>
      val sentence = FileManager.getCoNLLSentence(path).get
      val alignedTokens = for {
        w <- sentence.words
        t <- inflections.getAllForms(w.token.lowerCase)
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

  lazy val sourcedTokenizedQAPairsBySentence = {
    val shuffleRand = new util.Random(555555555L)
    questionInfos.groupBy(_._1).map {
      case (path, items) => path -> shuffleRand.shuffle(items.map(_._2))
    }
  }

  val numQAsPerHIT = 6

  lazy val prompts = {
    val shuffleRand = new util.Random(444443333L)

    val inOrder = for {
      (path, qaPairs) <- sourcedTokenizedQAPairsBySentence
      qaPairGroup <- qaPairs.grouped(numQAsPerHIT)
    } yield TokenizedValidationPrompt(path, qaPairGroup.toList)

    shuffleRand.shuffle(inOrder)
  }

  lazy val helper = new HITManager.Helper(taskSpec)
  lazy val hitManager = actorSystem.actorOf(Props(new NumAssignmentsHITManager(
    helper,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 100 else 3),
    prompts.iterator)))
  lazy val actor = actorSystem.actorOf(Props(new TaskManager(helper, hitManager)))

  import TaskManager._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    actor ! Start(interval)
  }
  def stop() = {
    actor ! Stop
  }
  def disable() = {
    actor ! Disable
  }
  def expire() = {
    actor ! Expire
  }
  def update() = {
    server
    actor ! Update
  }

  def completedRevisions = FileManager.loadAllHITInfo[TokenizedValidationPrompt, KeywordQuestionValidationResponse](taskSpec.hitTypeId)
    .filterNot(_.assignments.isEmpty)
    .map {
    case HITInfo(hit, assignments) =>
      val validatedQuestionLists = assignments.map(_.response.validatedQuestions).transpose
      (hit.prompt.path, hit.prompt.sourcedTokenizedQAPairs.zip(validatedQuestionLists))
  }.groupBy(_._1)
    .map {
    case (path, infosWithPath) => path -> infosWithPath.map(_._2).flatten
  }

  def printRevisions(revisions: Iterator[(CoNLLSentencePath, List[(SourcedTokenizedQAPair, List[ValidatedQuestion])])]) = {
    completedRevisions.foreach {
      case (path, validations) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        println("\n" + TextRendering.renderSentence(sentence))
        validations.foreach {
          case (stQAPair, validatedQs) =>
            val question = TextRendering.renderSentence(stQAPair.questionTokens)
            println(s"  $question")
            validatedQs.foreach {
              case InvalidQuestion =>
                println("    <Invalid>")
              case EditedQuestion(keywordIndex, newQuestion) =>
                val keywordToken = TextRendering.normalizeToken(sentence.words(keywordIndex).token)
                val keywordString = s"$keywordIndex:$keywordToken"
                println(f"    $keywordString%10s | $newQuestion%s")
            }
        }
    }
  }
}
