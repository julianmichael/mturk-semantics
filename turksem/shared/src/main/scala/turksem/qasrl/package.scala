package turksem

import nlpdata.datasets.wiktionary.InflectedForms

import turksem.qamr.GenerationPrompt
import turksem.qamr.WordedQAPair

package object qasrl {

  case class QASRLValidationPrompt[SID](
    genPrompt: GenerationPrompt[SID],
    sourceHITTypeId: String,
    sourceHITId: String,
    sourceAssignmentId: String,
    qaPairs: List[WordedQAPair]
  ) {
    def id = genPrompt.id
  }

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
