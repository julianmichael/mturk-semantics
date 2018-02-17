package example

import qasrl.crowd.QASRLEvaluationSettings

package object tqa {
  implicit val settings = new QASRLEvaluationSettings {
    override val invalidProportionEstimateLowerBound = .40
    override val invalidProportionEstimateUpperBound = .70
  }
}
