package turksem

import spacro.tasks.ResponseRW

import turksem.util._

import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags

package object iqa {

  val iqaTaskKey = "iqa"

  case class IQAPrompt[SID](id: SID)

  case class IQAResponse(qas: List[(InflectionalWord, TemplatedQA)])

  case class IQAAjaxRequest[SID](id: SID) {
    type Response = IQAAjaxResponse
  }
  object IQAAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] = new ResponseRW[IQAAjaxRequest[SID]] {
      override def getReader(request: IQAAjaxRequest[SID]) = implicitly[Reader[IQAAjaxResponse]]
      override def getWriter(request: IQAAjaxRequest[SID]) = implicitly[Writer[IQAAjaxResponse]]
    }
  }

  case class IQAAjaxResponse(
    inflectedTokens: Vector[InflectionalWord],
    questionGuesser: CountBasedQuestionGuesser)

  lazy val qaTemplatesToAlignments = allTemplates.all
    .flatMap(_.makeAllAlignedQATemplates)
    .groupBy(_.qaTemplate)

  lazy val allTemplates: TemplateRepo = {
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

    val optionalObject = List(List(NV), Nil)

    val objArgumentLists =
      List(List(NV, P), List(P, NV), List(NV), List(P), Nil)

    val adjectiveVerbArguments =
      List(List(J.reg), List(J.cmp), List(J.sup))

    val activeVerbs =
      List(
        List(optionalObject, adjectiveVerbArguments).sequence.map(_.flatten).distinct,
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

    // no passive verbs either

    TemplateRepo(
      verbTemplates = (activeVerbs ++ List(t(NV, V.pss))).map(Template(_)),
      adjectiveTemplates = adjectives.map(Template(_)),
      comparativeAdjectiveTemplates = comparatives.map(Template(_)),
      superlativeAdjectiveTemplates = superlatives.map(Template(_)),
      prepositionTemplates = prepositions.map(Template(_)),
      miscTemplateGroups = List(List(Template(t(NV, is, NV))))
    )
  }

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }
}
