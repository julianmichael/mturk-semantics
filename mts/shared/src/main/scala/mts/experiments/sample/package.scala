package mts.experiments

import mts.datasets.conll.CoNLLSentencePath
import mts.datasets.conll.CoNLLSentence

package object sample extends PackagePlatformExtensions {
  case class SamplePrompt(path: CoNLLSentencePath)
  case class SampleResponse(isGood: Boolean)

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(path: CoNLLSentencePath, sentence: CoNLLSentence) extends ApiResponse
}
