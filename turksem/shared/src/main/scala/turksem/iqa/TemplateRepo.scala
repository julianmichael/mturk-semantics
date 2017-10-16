package turksem.iqa

import turksem.util._

import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags

case class TemplateRepo(
  verbTemplates: List[Template],
  adjectiveTemplates: List[Template],
  comparativeAdjectiveTemplates: List[Template],
  superlativeAdjectiveTemplates: List[Template],
  prepositionTemplates: List[Template],
  miscTemplateGroups: List[List[Template]]) {
  lazy val all = List(
    verbTemplates,
    adjectiveTemplates,
    comparativeAdjectiveTemplates,
    superlativeAdjectiveTemplates,
    prepositionTemplates,
    miscTemplateGroups.flatten
  ).flatten

  def getTriggerIndex(template: Template): Option[Int] =
    if(verbTemplates.contains(template)) template.arguments.collectIndex {
      case AlignedVerb(_) => true
    } else if(
      adjectiveTemplates.contains(template) ||
        comparativeAdjectiveTemplates.contains(template) ||
        superlativeAdjectiveTemplates.contains(template)
    ) template.arguments.collectIndex {
      case Adjective(_) => true
    } else if(prepositionTemplates.contains(template)) template.arguments.collectIndex {
      case Preposition => true
    } else None

  def getTriggers(templates: List[Template]) = {
    templates.flatMap(t => getTriggerIndex(t).map(t -> _))
  }

  def getGroupForPosTag(posTag: String): Option[List[(Template, Int)]] = {
    if(PosTags.verbPosTags.contains(posTag)) Some(getTriggers(verbTemplates))
    else posTag match {
      case "JJ" => Some(getTriggers(adjectiveTemplates))
      case "JJR" => Some(getTriggers(comparativeAdjectiveTemplates))
      case "JJS" => Some(getTriggers(superlativeAdjectiveTemplates))
      case "IN" => Some(getTriggers(prepositionTemplates))
      case _ => None
    }
  }
}
