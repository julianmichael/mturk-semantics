package turksem

package object qasrl extends PackagePlatformExtensions {

  type QASRLGenerationApiRequest[SID] = qamr.annotation.GenerationPrompt[SID]

  case class IndexWithTemplate(
    wordIndex: Int,
    template: QASRLTemplate)

  case class QASRLGenerationApiResponse(
    tokens: Vector[String],
    templates: List[IndexWithTemplate])

}
