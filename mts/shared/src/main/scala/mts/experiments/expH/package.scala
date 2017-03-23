package mts.experiments

import mts.datasets.ptb._
import mts.datasets.wiki1k._
import mts.language._
import mts.util._

package object expH extends PackagePlatformExtensions {
  sealed trait SentenceId {
    def readableFileString = this match {
      case PTBSentenceId(path) => s"PTB:${path.filePath.suffix}"
      case WikiSentenceId(path) => s"Wiki1k:${path.filePath.get}"
    }
    def readableSentenceIndex = this match {
      case PTBSentenceId(path) => s"${path.sentenceNum}"
      case WikiSentenceId(path) => s"${path.paragraphNum}:${path.sentenceNum}"
    }
  }
  case class PTBSentenceId(path: PTBSentencePath) extends SentenceId
  case class WikiSentenceId(path: Wiki1kSentencePath) extends SentenceId

  def getPTBSentenceTokens(sentence: PTBSentence): Vector[String] = {
    sentence.words.filter(_.pos != "-NONE-").map(_.token)
  }

  def renderValidationAnswer(
    sentence: Vector[String],
    va: ValidationAnswer,
    referenceQAs: List[WordedQAPair]
  ): String = va match {
    case InvalidQuestion => "<Invalid>"
    case Redundant(i) => s"<Redundant with ${referenceQAs(i).question}>"
    case Answer(span) => TextRendering.renderSpan(sentence, span)
  }

  val finalExperimentName = "h_final"

  val generationReward = 0.20
  val bonusIncrement = 0.03
  def bonusFor(i: Int): Double = bonusIncrement * i + 0.03
  def generationBonus(nKeywords: Int, nValidQAs: Int) =
    math.max(0.0, (1 to (nValidQAs - nKeywords)).map(bonusFor).sum)
  val numKeywords = 4
  val questionCharLimit = 50

  val validationReward = 0.08
  val validationBonusPerQuestion = 0.02
  val validationBonusThreshold = numKeywords
  def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  def numValidQuestions(responses: List[List[ValidationAnswer]]) =
    math.round(responses.map(_.filter(_.isAnswer).size).mean - 0.01).toInt

  val generationAccuracyWarningThreshold = 0.85
  val generationAccuracyBlockingThreshold = 0.8
  val generationBufferBeforeWarning = 10
  val generationBufferBeforeBlocking = 10

  val validationAgreementWarningThreshold = 0.75
  val validationAgreementBlockingThreshold = 0.70
  val validationBufferBeforeWarning = 10
  val validationBufferBeforeBlocking = 10

  case class GenerationPrompt(
    id: SentenceId,
    keywords: List[Int])

  // List[WordedQAPair] is response for qa gen
  case class WordedQAPair(wordIndex: Int, question: String, answer: Set[Int])

  case class GenerationApiRequest(id: SentenceId)
  case class GenerationApiResponse(tokens: Vector[String])

  // prompt for validation
  case class ValidationPrompt(
    genPrompt: GenerationPrompt,
    sourceHITId: String,
    sourceAssignmentId: String,
    qaPairs: List[WordedQAPair]
  ) {
    def id = genPrompt.id
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

  def resolveRedundancy(va: ValidationAnswer, answers: List[ValidationAnswer]) =
    va.getRedundant.fold(va)(r => answers(r.other))

  import mts.core._
  def numAgreed(
    a1: Assignment[List[ValidationAnswer]],
    a2: Assignment[List[ValidationAnswer]]
  ) = {
    a1.response.map(resolveRedundancy(_, a1.response)).zip(
      a2.response.map(resolveRedundancy(_, a2.response))).filter {
      case (InvalidQuestion, InvalidQuestion) => true
      case (Answer(span1), Answer(span2)) => !span1.intersect(span2).isEmpty
      case _ => false
    }.size
  }


  case class ValidationApiRequest(id: SentenceId)
  case class ValidationApiResponse(sentence: Vector[String])
}
