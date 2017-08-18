package turksem

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.structure.Word

/**
  * Data types common to server and client when doing the SciSRL task.
  */
package object scisrl {

  // input to the task
  case class SciSRLPrompt[SID](sentenceId: SID, verbIndices: List[Int])

  // TODO
  // output of the task that turkers produce
  case class SciSRLResponse()

  // request type for the task client to send the server over websockets
  case class SciSRLApiRequest[SID](prompt: SciSRLPrompt[SID])

  // response that the serve can send the task client over websockets
  case class SciSRLApiResponse(
    posTaggedTokens: Vector[Word],
    verbInflectedForms: List[InflectedForms])

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
