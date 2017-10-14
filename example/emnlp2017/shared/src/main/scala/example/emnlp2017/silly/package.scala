package example.emnlp2017

import cats.implicits._

import nlpdata.util.LowerCaseStrings._

package object silly {
  type Template = List[Token]
  type TemplateGroup = List[Template]

  val templates: List[TemplateGroup] = {
    import scala.language.implicitConversions
    def t(tokens: Token*): Template = tokens.toList
    def g(tokens: Token*): List[Template] = List(tokens.toList)
    implicit def str2Token(s: String) = StringToken(s.lowerCase)
    import NounConstraint._
    import VerbVoice._
    val N = Noun; val V = AlignedVerb; val P = Preposition
    val J = Adjective; val is = Copula; val NV = N.dg

    val obliqueArgumentLists = List(
      List(List(P, NV), List(P), Nil),
      List(List(P, NV), List(P), Nil),
      List(List(ToVerb), Nil)
    ).sequence.map(_.flatten)

    val nounPrep = List(
      t(NV, is, P, NV),
      t(NV, is, P)
    )

    val adjPrefix = t(NV, is, J.reg)
    val adjectives = obliqueArgumentLists.map(adjPrefix ++ _)

    val comparatives = adjectives.map { template =>
      template.map {
        case Adjective(_) => Adjective.cmp
        case x => x
      } ++ t("than", NV)
    }

    val superlatives = adjectives.map { template =>
      template.map {
        case Adjective(_) => Adjective.sup
        case x => x
      }
    }

    val objArgumentLists = List(
      List(List(NV, P), List(P, NV), List(NV), List(P), Nil),
      List(List(NV), List(J.reg), List(J.cmp), List(J.sup), Nil)
    ).sequence.map(_.flatten)

    val activeVerbs = (
      for {
        obj <- objArgumentLists
        obl <- obliqueArgumentLists
      } yield t(NV, V.act) ++ obj ++ obl)

    val passiveVerbs = obliqueArgumentLists.map(t(NV, V.pss) ++ _)

    List(
      g("which", N.bare),
      g("whose", N.bare),
      g("how", "many", N.bare),
      g("where", is, N.det),
      g("how", is, N.det),
      g(NV, is, NV),
      nounPrep,
      g("how", J.reg),
      adjectives,
      g("how", "much", J.cmp),
      comparatives,
      superlatives,
      activeVerbs,
      passiveVerbs
    )
  }
}
