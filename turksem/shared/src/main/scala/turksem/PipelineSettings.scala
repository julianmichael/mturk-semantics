package turksem

trait PipelineSettings {
  val generationReward: Double
  val bonusIncrement: Double
  def bonusFor(i: Int): Double

  final def generationBonus(nKeywords: Int, nValidQAs: Int) =
    math.max(0.0, (1 to (nValidQAs - nKeywords)).map(bonusFor).sum)

  val numKeywords: Int
  val questionCharLimit: Int

  val validationReward: Double
  val validationBonusPerQuestion: Double
  val validationBonusThreshold: Int

  final def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  val generationAccuracyWarningThreshold: Double
  val generationAccuracyBlockingThreshold: Double
  val generationBufferBeforeWarning: Int
  val generationBufferBeforeBlocking: Int

  val validationAgreementWarningThreshold: Double
  val validationAgreementBlockingThreshold: Double
  val validationBufferBeforeWarning: Int
  val validationBufferBeforeBlocking: Int

}
