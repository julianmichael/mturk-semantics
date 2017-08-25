package turksem.qasrl

import turksem.qamr.GenerationPrompt

case class QASRLValidationResult[SID](
  prompt: GenerationPrompt[SID],
  sourceHITTypeId: String,
  sourceHITId: String,
  sourceAssignmentId: String,
  numValid: Int)
