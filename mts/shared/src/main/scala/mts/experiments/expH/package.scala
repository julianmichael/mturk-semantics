package mts.experiments

import mts.datasets.ptb._
import mts.language._
import mts.util._

package object expH extends PackagePlatformExtensions {
  def getPTBSentenceTokens(sentence: PTBSentence): Vector[String] = {
    sentence.words.filter(_.pos != "-NONE-").map(_.token)
  }

  def renderValidationAnswer(
    sentence: PTBSentence,
    va: ValidationAnswer,
    referenceQAs: List[WordedQAPair]
  ): String = va match {
    case InvalidQuestion => "<Invalid>"
    case Redundant(i) => s"<Redundant with ${referenceQAs(i).question}>"
    case Answer(span) => TextRendering.renderSpan(sentence, span)
  }

  val finalExperimentName = "h_final"

  val generationReward = 0.20
  def bonusFor(i: Int): Double = 0.01 * i + 0.04
  def generationBonus(nKeywords: Int, nValidQAs: Int) =
    math.max(0.0, (1 to (nValidQAs - nKeywords)).map(bonusFor).sum)
  val numKeywords = 4
  val questionCharLimit = 50

  val validationReward = 0.15
  val validationBonusPerQuestion = 0.03
  val validationBonusThreshold = numKeywords
  def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  def numValidQuestions(responses: List[List[ValidationAnswer]]) =
    math.round(responses.map(_.filter(_.isAnswer).size).mean - 0.01).toInt

  val generationAccuracyThreshold = 0.7
  val generationBufferBeforeWarning = 30
  val generationBufferBeforeBlocking = 15

  val validationAgreementThreshold = 0.7
  val validationBufferBeforeWarning = 30
  val validationBufferBeforeBlocking = 15

  case class GenerationPrompt(
    path: PTBSentencePath,
    keywords: List[Int])

  // List[WordedQAPair] is response for qa gen
  case class WordedQAPair(wordIndex: Int, question: String, answer: Set[Int])

  case class GenerationApiRequest(path: PTBSentencePath)
  case class GenerationApiResponse(tokens: Vector[String])

  // prompt for validation
  case class ValidationPrompt(
    genPrompt: GenerationPrompt,
    sourceHITId: String,
    sourceAssignmentId: String,
    qaPairs: List[WordedQAPair]
  ) {
    def path = genPrompt.path
  }

  // List[ValidationAnswer] is response for validation
  sealed trait ValidationAnswer {
    def isInvalid = this match {
      case InvalidQuestion => true
      case _ => false
    }

    def getRedundant = this match {
      case r @ Redundant(_) => Some(r)
      case _ => None
    }
    def isRedundant = !getRedundant.isEmpty

    def getAnswer = this match {
      case a @ Answer(_) => Some(a)
      case _ => None
    }
    def isAnswer = !getAnswer.isEmpty

    def isComplete = this match {
      case InvalidQuestion => true
      case Redundant(_) => true
      case Answer(indices) => !indices.isEmpty
    }
  }
  case object InvalidQuestion extends ValidationAnswer
  case class Redundant(other: Int) extends ValidationAnswer
  case class Answer(indices: Set[Int]) extends ValidationAnswer

  case class ValidationApiRequest(path: PTBSentencePath)
  case class ValidationApiResponse(sentence: Vector[String])
}
