package turksem

import nlpdata.datasets.wiktionary.InflectedForms

package object qasrl {

  case class QASRLGenerationApiRequest[SID](prompt: qamr.GenerationPrompt[SID])

  case class IndexWithInflectedForms(
    wordIndex: Int,
    inflectedForms: InflectedForms)

  case class QASRLGenerationApiResponse(
    tokens: Vector[String],
    templates: List[IndexWithInflectedForms])

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }

}
