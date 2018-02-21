package example

import qasrl.crowd.QASRLEvaluationSettings

package object tqa {
  implicit val settings = new QASRLEvaluationSettings {
    // TODO figure this sheet out
    override val invalidProportionEstimateLowerBound = .10
    override val invalidProportionEstimateUpperBound = .40

    override val validationAgreementGracePeriod = 35

    override val validationReward = 0.10
    override val validationBonusPerQuestion = 0.02
    override val validationBonusThreshold = 5
  }
}
