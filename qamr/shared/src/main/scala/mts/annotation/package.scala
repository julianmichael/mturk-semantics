package mts

import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._
import nlpdata.util.Text

import mts._
import mts.util._
import turkey._

package object annotation extends PackagePlatformExtensions {
  val expHGenerationTaskKey = "expHGeneration"
  val expHValidationTaskKey = "expHValidation"
  val expHDashboardTaskKey = "expHDashboard"

  def renderValidationAnswer(
    sentence: Vector[String],
    va: ValidationAnswer,
    referenceQAs: List[WordedQAPair]
  ): String = va match {
    case InvalidQuestion => "<Invalid>"
    case Redundant(i) => s"<Redundant with ${referenceQAs(i).question}>"
    case Answer(span) => Text.renderSpan(sentence, span)
  }

  val finalExperimentName = "h_final"

  val generationReward = 0.20
  val bonusIncrement = 0.03
  def bonusFor(i: Int): Double = bonusIncrement * i + 0.03
  def generationBonus(nKeywords: Int, nValidQAs: Int) =
    math.max(0.0, (1 to (nValidQAs - nKeywords)).map(bonusFor).sum)
  val numKeywords = 4
  val questionCharLimit = 50

  val validationReward = 0.10
  val validationBonusPerQuestion = 0.02
  val validationBonusThreshold = numKeywords
  def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  def numValidQuestions(responses: List[List[ValidationAnswer]]) =
    math.round(responses.map(_.filter(_.isAnswer).size).mean - 0.01).toInt

  val generationAccuracyWarningThreshold = 0.8
  val generationAccuracyBlockingThreshold = 0.75
  val generationBufferBeforeWarning = 10
  val generationBufferBeforeBlocking = 10

  val validationAgreementWarningThreshold = 0.75
  val validationAgreementBlockingThreshold = 0.70
  val validationBufferBeforeWarning = 10
  val validationBufferBeforeBlocking = 10

  case class GenerationPrompt[SID](
    id: SID,
    keywords: List[Int])

  type GenerationResponse = List[WordedQAPair]

  case class GenerationApiRequest[SID](id: SID)
  case class GenerationApiResponse(tokens: Vector[String])

  // prompt for validation
  case class ValidationPrompt[SID](
    genPrompt: GenerationPrompt[SID],
    sourceHITId: String,
    sourceAssignmentId: String,
    qaPairs: List[WordedQAPair]
  ) {
    def id = genPrompt.id
  }

  type ValidationResponse = List[ValidationAnswer]

  case class ValidationApiRequest[SID](id: SID)
  case class ValidationApiResponse(sentence: Vector[String])
}
