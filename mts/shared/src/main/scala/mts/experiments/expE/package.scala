package mts.experiments

import mts.conll.CoNLLSentencePath
import mts.conll.CoNLLSentence

package object expE extends PackagePlatformExtensions {
  case class QAGenPrompt(path: CoNLLSentencePath, wordIndex: Int)
  case class QAGenResponse(qaPairs: List[(String, Set[Int])])

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(path: CoNLLSentencePath, sentence: CoNLLSentence) extends ApiResponse

  val bonuses = List(0.0, 0.03, 0.06, 0.09, 0.12)
  val numQAs = bonuses.size
}
