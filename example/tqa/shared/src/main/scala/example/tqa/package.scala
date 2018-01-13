package example

import qasrl.crowd.QASRLSettings

package object tqa {
  implicit val settings = new QASRLSettings {
    override val generationCoverageQuestionsPerVerbThreshold = 1.8
  }
}
