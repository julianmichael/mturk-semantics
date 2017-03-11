package mts.experiments.expH

import mts.core._
import mts.datasets.ptb.PTBSentence

case class ValidatedAssignment(
  genHIT: HIT[GenerationPrompt],
  genAssignment: Assignment[List[WordedQAPair]],
  valAssignments: List[Assignment[List[ValidationAnswer]]]) {
  def genCost: Double = {
    val numValidQs = numValidQuestions(valAssignments.map(_.response))
    generationReward + generationBonus(genHIT.prompt.keywords.size, numValidQs)
  }
  def valCost: Double = valAssignments.size * (validationReward + validationBonus(genAssignment.response.size))
}

case class SentenceHITInfo(
  sentence: PTBSentence,
  genHITInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]],
  valHITInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]) {

  def alignValidations: List[ValidatedAssignment] = for {
    HITInfo(genHIT, genAssignments) <- genHITInfos
    genAssignment <- genAssignments
    HITInfo(valHIT, valAssignments) <- valHITInfos
    if valHIT.prompt.sourceHITId == genHIT.hitId && valHIT.prompt.sourceAssignmentId == genAssignment.assignmentId
  } yield ValidatedAssignment(genHIT, genAssignment, valAssignments)
}

case class SummaryInfo(
  // generation
  val numGenActive: Int,
  val genWorkerStats: Map[String, WorkerStats],
  val genFeedback: List[String],
  // validation
  val numValPromptsWaiting: Int,
  val numValActive: Int,
  val valWorkerInfo: Map[String, WorkerInfo],
  val valFeedback: List[String],
  // final results
  val lastFewSentences: Map[SentenceStats, SentenceHITInfo],
  val aggSentenceStats: AggregateSentenceStats)
