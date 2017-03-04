package mts.experiments

import mts.ptb.PTBSentencePath

package object expH extends PackagePlatformExtensions {

  val bonuses = List(0.0, 0.04, 0.05, 0.06, 0.07)
  val numQAs = bonuses.size
  val numKeywords = 4
  val questionCharLimit = 50

  val numValidationQAs = 6

  // see WordStats.scala for functions on these
  type WordStats = Map[Int, WordStat]

  // prompt for qa gen
  sealed trait SentenceId
  case class PTBSentenceId(path: PTBSentencePath)
  case class CoNLLSentenceId(path: CoNLLSentencePath) extends SentenceId // TODO remove
  // case class WikiSentenceId(locator: String) extends SentenceId // TODO

  case class GenerationPrompt(
    path: PTBSentencePath,
    keywords: List[Int])

  // List[WordedQAPair] is response for qa gen
  case class WordedQAPair(wordIndex: Int, question: String, answer: Set[Int])

  case class GenerationApiRequest(identifier: SentenceId)
  case class GenerationApiResponse(tokens: Vector[String], wordStats: WordStats)

  case class SourcedQAPair(hitId: String, assignmentId: String, index: Int, qaPair: WordedQAPair) {
    def wordIndex = qaPair.wordIndex
    def question = qaPair.question
    def answer = qaPair.answer
  }

  // prompt for validation
  case class ValidationPrompt(
    sentenceId: PTBSentencePath,
    qaPairs: List[SourcedQAPair]
  )

  // List[ValidationAnswer] is response for validation
  sealed trait ValidationAnswer
  case object InvalidQuestion extends ValidationAnswer
  case class Answer(indices: Set[Int]) extends ValidationAnswer

  case class ValidationApiRequest(identifier: SentenceId)
  case class ValidationApiResponse(sentence: Vector[String])
}
