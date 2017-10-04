package example

import turksem.qasrl.QASRLSettings

package object tqa {
  implicit val settings = new QASRLSettings {
    override val generationCoverageQuestionsPerVerbThreshold = 1.6
  }
}
