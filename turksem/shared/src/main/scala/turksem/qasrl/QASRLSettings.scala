package turksem.qasrl

import turksem.PipelineSettings

object QASRLSettings extends PipelineSettings {

  // used as URL parameters that indicate to the client which interface to use

  val generationTaskKey = "generation"
  val validationTaskKey = "validation"
  val dashboardTaskKey = "dashboard"

  // annotation pipeline hyperparameters

  override val generationReward = 0.20
  override val bonusIncrement = 0.03
  override def bonusFor(i: Int): Double = bonusIncrement * i + 0.03

  override val numKeywords = 4
  override val questionCharLimit = 50

  override val validationReward = 0.10
  override val validationBonusPerQuestion = 0.02
  override val validationBonusThreshold = numKeywords

  override val generationAccuracyWarningThreshold = 0.8
  override val generationAccuracyBlockingThreshold = 0.75
  override val generationBufferBeforeWarning = 10
  override val generationBufferBeforeBlocking = 10

  override val validationAgreementWarningThreshold = 0.75
  override val validationAgreementBlockingThreshold = 0.70
  override val validationBufferBeforeWarning = 10
  override val validationBufferBeforeBlocking = 10
}
