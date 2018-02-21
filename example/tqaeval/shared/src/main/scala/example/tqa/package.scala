package example

import qasrl.crowd.QASRLEvaluationSettings

package object tqa {
  implicit val settings = new QASRLEvaluationSettings {
    override val invalidProportionEstimateLowerBound = .20
    override val invalidProportionEstimateUpperBound = .50

    override val validationAgreementGracePeriod = 35

    override val validationReward = 0.08
    override val validationBonusPerQuestion = 0.02
    override val validationBonusThreshold = 4
  }
}
