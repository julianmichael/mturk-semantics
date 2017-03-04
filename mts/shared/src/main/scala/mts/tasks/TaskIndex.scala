package mts.tasks

import upickle.default._

// == THE TASK INDEX ==
// you must place a label for your task here
object TaskIndex {
  val sampleTaskKey = "sample"
  val expEQAGenTaskKey = "expEQAGen"
  val expFAnswerValidationTaskKey = "expFAnswerValidation"
  val expFQuestionValidationTaskKey = "expFQuestionValidation"
  val expFLongAnswerValidationTaskKey = "expFLongAnswerValidation"
  val expGWordLimQValTaskKey = "expGWordLimQVal"
  val expGManualQATaskKey = "expGManualQA"
  val expHGenerationTaskKey = "expHGeneration"
  val expHValidationTaskKey = "expHValidation"
}
