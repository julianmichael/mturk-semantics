package example.emnlp2017

import turksem.util._

import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags

package object silly {

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

  lazy val templates: TemplateRepo = {
    import scala.language.implicitConversions
    def t(tokens: Token*): List[Token] = tokens.toList
    def g(tokens: Token*): List[List[Token]] = List(tokens.toList)
    implicit def str2Token(s: String) = StringToken(s.lowerCase)
    import NounConstraint._
    import VerbVoice._
    val N = Noun; val V = AlignedVerb; val P = Preposition
    val J = Adjective; val is = Copula; val NV = N.dg

    val obliqueArgumentLists = List(
      List(P, NV), List(P), List(ToVerb), Nil
    )

    val prepositions = obliqueArgumentLists.filter(_.nonEmpty).map(t(NV, is) ++ _)

    val adjectives = obliqueArgumentLists.map(t(NV, is, J.reg) ++ _)

    val comparatives = {
      val justComparatives = adjectives.map { template =>
        template.map {
          case Adjective(_) => Adjective.cmp
          case x => x
        }
      }
      justComparatives ++ (justComparatives.map(_ ++ t("than", NV)))
    }

    val superlatives = adjectives.map { template =>
      template.map {
        case Adjective(_) => Adjective.sup
        case x => x
      }
    }

    val objArgumentLists =
      List(List(NV, P), List(P, NV), List(NV), List(P), Nil)

    val adjectiveVerbArguments = List(
      List(List(NV), Nil),
      List(List(J.reg), List(J.cmp), List(J.sup))
    ).sequence.map(_.flatten).distinct

    val activeVerbs =
      List(
        adjectiveVerbArguments,
        List(objArgumentLists, obliqueArgumentLists).sequence.map(_.flatten).distinct
      ).flatten.map(t(NV, V.act) ++ _)

    val passiveVerbs = List(
      adjectiveVerbArguments,
      obliqueArgumentLists
    ).flatten.map(t(NV, V.pss) ++ _)

    // TODO waiting on these which are less reliable templates anyway
    // g("which", N.bare),
    // g("whose", N.bare),
    // g("how", "many", N.bare),
    // g("where", is, N.det),
    // g("how", is, N.det),
    // g("how", J.reg),
    // g("how", "much", J.cmp),

    TemplateRepo(
      verbTemplates = (activeVerbs ++ passiveVerbs).map(Template(_)),
      adjectiveTemplates = adjectives.map(Template(_)),
      comparativeAdjectiveTemplates = comparatives.map(Template(_)),
      superlativeAdjectiveTemplates = superlatives.map(Template(_)),
      prepositionTemplates = prepositions.map(Template(_)),
      miscTemplateGroups = List(List(Template(t(NV, is, NV))))
    )
  }
}
