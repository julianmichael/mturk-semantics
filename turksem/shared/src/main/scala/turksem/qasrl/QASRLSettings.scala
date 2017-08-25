package turksem.qasrl

object QASRLSettings {

  // used as URL parameters that indicate to the client which interface to use

  val generationTaskKey = "generation"
  val validationTaskKey = "validation"
  val dashboardTaskKey = "dashboard"

  final def generationBonus(nKeywords: Int, nValidQAs: Int) = {
    if(nKeywords < 3) {
      math.max(0.0, (nValidQAs - 1) * bonusPerQuestion) // bonus applies after 1 question
    } else {
      math.max(0.0, (nValidQAs - 3) * bonusPerQuestion) // bonus applies after 3 questions
    }
  }

  final def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  // annotation pipeline hyperparameters

  val smallGenerationReward = 0.05
  val largeGenerationReward = 0.15
  val bonusPerQuestion = 0.06

  val validationReward = 0.08
  val validationBonusPerQuestion = 0.02
  val validationBonusThreshold = 4

  val generationAccuracyWarningThreshold = 0.8
  val generationAccuracyBlockingThreshold = 0.75
  val generationBufferBeforeWarning = 10
  val generationBufferBeforeBlocking = 10

  val validationAgreementWarningThreshold = 0.75
  val validationAgreementBlockingThreshold = 0.70
  val validationBufferBeforeWarning = 10
  val validationBufferBeforeBlocking = 10
}
