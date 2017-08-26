package turksem

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.structure.Word

/**
  * Data types common to server and client when doing the SciSRL task.
  */
package object scisrl {

  // input to the task
  case class SciSRLPrompt[SID](
    sentenceId: SID,
    verbIndices: List[Int],
    posTaggedTokens: Vector[Word],
    verbInflectedForms: List[InflectedForms])

  // represents each span as its set of indices in the sentence
  case class Proposition(
    subj: Option[Set[Int]],
    obj: Option[Set[Int]],
    loc: Option[Set[Int]],
    time: Option[Set[Int]])

  // output of the task that turkers produce
  case class SciSRLResponse(
    propositions: List[Proposition],
    enablers: Set[(Int, Int)], // (i, j) -> propositions(i) enables propositions(j)
    preventers: Set[(Int, Int)]) // (i, j) -> propositions(i) prevents propositions(j)

  // We need custom picklers since LowerCaseString is an opaque-sealed type.
  // In the future we can get them from proper subst through String.
  // For that, I need to work cats.As or Is (and maybe use newts) in the definition of LowerCaseString inside nlpdata.
  // Anyway, we need these picklers for the compiler to be able to synthesize
  // picklers for InflectedForms (part of the API response type above) since LowerCaseString is a field of InflectedForms.

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }
}
