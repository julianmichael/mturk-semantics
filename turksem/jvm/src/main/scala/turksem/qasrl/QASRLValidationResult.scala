package turksem.qasrl

case class QASRLValidationResult[SID](
  prompt: QASRLGenerationPrompt[SID],
  sourceHITTypeId: String,
  sourceHITId: String,
  sourceAssignmentId: String,
  numValid: Int)
