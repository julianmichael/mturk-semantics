package turksem.qasrl

object QASRLSettings {

  // used as URL parameters that indicate to the client which interface to use

  val generationTaskKey = "generation"
  val validationTaskKey = "validation"
  val dashboardTaskKey = "dashboard"

  // annotation pipeline hyperparameters

  val generationRewardCents = 5
  val generationReward = generationRewardCents * 0.01

  final def generationBonus(nValidQAs: Int) = {
    // no bonus for the first question, hence -1
    val cents = (0 until (nValidQAs - 1)).map(_ + generationRewardCents).sum
    cents * 0.01
  }

  val validationReward = 0.08
  val validationBonusPerQuestion = 0.02
  val validationBonusThreshold = 4

  final def validationBonus(numQuestions: Int) =
    math.max(0.0, validationBonusPerQuestion * (numQuestions - validationBonusThreshold))

  val generationCoverageQuestionsPerVerbThreshold = 2.0
  val generationCoverageBlockingThreshold = math.floor(generationCoverageQuestionsPerVerbThreshold * 10).toInt
  val generationCoverageGracePeriod = 15

  val generationAccuracyBlockingThreshold = 0.75
  val generationAccuracyGracePeriod = 15

  val validationAgreementBlockingThreshold = 0.70
  val validationAgreementGracePeriod = 15
}
