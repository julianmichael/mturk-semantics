package turksem.qasrl

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.Inflections

trait PackagePlatformExtensions {

  // TODO move to shared QuestionTemplate code once Inflections is cross-platform
  def makeTemplateFromTokensWithVerb(
    tokens: Vector[String],
    verbIndex: Int)(
    implicit inflections: Inflections
  ): Option[QASRLTemplate] =
    inflections.getWellTypedInflectedForms(tokens(verbIndex).lowerCase).map(forms =>
      QASRLTemplate(
        Slots.getVerbSlotChoices(forms),
        Slots.getPrepositionSlotChoices(tokens)
      )
    )

}
