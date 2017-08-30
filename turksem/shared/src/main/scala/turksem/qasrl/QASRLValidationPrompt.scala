package turksem.qasrl

import turksem.qamr.GenerationPrompt

case class QASRLValidationPrompt[SID](
  genPrompt: GenerationPrompt[SID],
  sourceHITTypeId: String,
  sourceHITId: String,
  sourceAssignmentId: String,
  qaPairs: List[VerbQA]
) {
  def id = genPrompt.id
}
