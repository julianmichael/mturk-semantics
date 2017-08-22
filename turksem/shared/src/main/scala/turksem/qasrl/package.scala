package turksem

package object qasrl extends PackagePlatformExtensions {

  type QASRLGenerationPrompt[SID] = qamr.GenerationPrompt[SID]
  val QASRLGenerationPrompt = qamr.GenerationPrompt

  type QASRLGenerationApiRequest[SID] = qamr.GenerationPrompt[SID]

  case class IndexWithTemplate(
    wordIndex: Int,
    template: QASRLTemplate)

  case class QASRLGenerationApiResponse(
    tokens: Vector[String],
    templates: List[IndexWithTemplate])

}
