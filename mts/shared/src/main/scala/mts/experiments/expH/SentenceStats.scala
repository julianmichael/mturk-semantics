package mts.experiments.expH

import mts.core._
import mts.util._
import mts.datasets.ptb._

case class SentenceStatus(
  id: SentenceId,
  allKeywords: Set[Int],
  keywordsFinished: Set[Int],
  ongoingValidations: Set[ValidationPrompt],
  finishedAssignments: List[Assignment[List[ValidationAnswer]]]) {
  def isFinished = {
    val keywordsRemaining = allKeywords -- keywordsFinished
    keywordsRemaining.isEmpty && ongoingValidations.isEmpty
  }

  def withKeywords(keywords: Set[Int]) = this.copy(
    keywordsFinished = this.keywordsFinished ++ keywords
  )

  def beginValidation(vPrompt: ValidationPrompt) = this.copy(
    ongoingValidations = this.ongoingValidations + vPrompt
  )

  def finishValidation(vPrompt: ValidationPrompt, assignments: List[Assignment[List[ValidationAnswer]]]) = this.copy(
    ongoingValidations = this.ongoingValidations - vPrompt,
    finishedAssignments = assignments ++ this.finishedAssignments
  )
}

case class SentenceStats(
  id: SentenceId,
  earliestTime: Long,
  latestTime: Long,
  // numWords: Int // TODO
  numKeywords: Int,
  numQAPairs: Int,
  numValidQAPairs: Int,
  numQAPairsEachKeywordPrompt: List[Int],
  numQAPairsEachKeywordActual: List[Int],
  validationLatencies: List[Int], // seconds
  completionTime: Long,
  generationCost: Double,
  validationCost: Double,
  genHITIds: Set[String],
  valHITIds: Set[String])

case class AggregateSentenceStats(
  earliestTime: Long,
  latestTime: Long,
  numSentences: Int,
  numKeywords: Int,
  numQAPairs: Int,
  numValidQAPairs: Int,
  keywordPromptQAPairHist: IntHist,
  keywordActualQAPairHist: IntHist,
  validationLatencyHist: IntHist,
  generationCost: Double,
  validationCost: Double) {

  def add(stats: SentenceStats) = this.copy(
    earliestTime = math.min(this.earliestTime, stats.earliestTime),
    latestTime = math.max(this.latestTime, stats.latestTime),
    numSentences = this.numSentences + 1,
    numKeywords = this.numKeywords + stats.numKeywords,
    numQAPairs = this.numQAPairs + stats.numQAPairs,
    numValidQAPairs = this.numValidQAPairs + stats.numValidQAPairs,
    keywordPromptQAPairHist = this.keywordPromptQAPairHist.addAll(stats.numQAPairsEachKeywordPrompt),
    keywordActualQAPairHist = this.keywordActualQAPairHist.addAll(stats.numQAPairsEachKeywordActual),
    validationLatencyHist = this.validationLatencyHist.addAll(stats.validationLatencies),
    generationCost = this.generationCost + stats.generationCost,
    validationCost = this.validationCost + stats.validationCost)
}
object AggregateSentenceStats {
  def empty = AggregateSentenceStats(
    0L, 0L,
    0, 0, 0, 0,
    IntHist.empty, IntHist.empty, IntHist.empty,
    0.0, 0.0)

  def aggregate(ss: List[SentenceStats]) = ss.foldLeft(empty)(_ add _)
}

