package mts.experiments

import mts.conll.CoNLLSentencePath
import mts.conll.CoNLLSentence

import expE.QAGenPrompt
import expE.QAGenResponse

package object expF extends PackagePlatformExtensions {
  case class Prompt(origPrompt: QAGenPrompt, origResponse: QAGenResponse)
  case class Response(qaPairs: List[(String, Set[Int])])

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(path: CoNLLSentencePath, sentence: CoNLLSentence) extends ApiResponse

  val bonuses = List(0.0, 0.03, 0.04, 0.05, 0.06)
}

