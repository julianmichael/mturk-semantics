package turksem

import nlpdata.datasets.wiktionary.InflectedForms

import turksem.qamr.WorkerStats

package object qasrl {

  case class GenerationStatSummary(
    numVerbsCompleted: Int, // before validation: used to calculate coverage
    numQuestionsWritten: Int, // before validation: "
    workerStatsOpt: Option[WorkerStats])

  case class QASRLGenerationApiRequest[SID](
    workerIdOpt: Option[String],
    prompt: qamr.GenerationPrompt[SID]
  )

  case class IndexWithInflectedForms(
    wordIndex: Int,
    inflectedForms: InflectedForms)

  case class QASRLGenerationApiResponse(
    stats: GenerationStatSummary,
    tokens: Vector[String],
    templates: List[IndexWithInflectedForms])

  case class QASRLValidationApiRequest[SID](
    workerIdOpt: Option[String],
    id: SID)

  case class QASRLValidationApiResponse(
    workerInfoOpt: Option[QASRLValidationWorkerInfo],
    sentence: Vector[String])

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }

}
