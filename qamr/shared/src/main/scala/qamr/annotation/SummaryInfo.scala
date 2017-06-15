package qamr.annotation

import qamr._
import turkey._

case class ValidatedAssignment[SID](
  genHIT: HIT[GenerationPrompt[SID]],
  genAssignment: Assignment[List[WordedQAPair]],
  valAssignments: List[Assignment[List[ValidationAnswer]]]) {
  def genCost: Double = {
    val numValidQs = numValidQuestions(valAssignments.map(_.response))
    generationReward + generationBonus(genHIT.prompt.keywords.size, numValidQs)
  }
  def valCost: Double = valAssignments.size * (validationReward + validationBonus(genAssignment.response.size))
}

case class SentenceHITInfo[SID](
  tokens: Vector[String],
  genHITInfos: List[HITInfo[GenerationPrompt[SID], List[WordedQAPair]]],
  valHITInfos: List[HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]]) {

  def alignValidations: List[ValidatedAssignment[SID]] = for {
    HITInfo(genHIT, genAssignments) <- genHITInfos
    genAssignment <- genAssignments
    HITInfo(valHIT, valAssignments) <- valHITInfos
    if valHIT.prompt.sourceHITId == genHIT.hitId && valHIT.prompt.sourceAssignmentId == genAssignment.assignmentId
  } yield ValidatedAssignment(genHIT, genAssignment, valAssignments)
}

case class SummaryInfo[SID](
  // generation
  val numGenActive: Int,
  val genWorkerStats: Map[String, WorkerStats],
  val genFeedback: List[Assignment[List[WordedQAPair]]],
  // validation
  val numValPromptsWaiting: Int,
  val numValActive: Int,
  val valWorkerInfo: Map[String, WorkerInfo],
  val valFeedback: List[Assignment[List[ValidationAnswer]]],
  // final results
  val lastFewSentences: Map[SentenceStats[SID], SentenceHITInfo[SID]],
  val aggSentenceStats: AggregateSentenceStats)
