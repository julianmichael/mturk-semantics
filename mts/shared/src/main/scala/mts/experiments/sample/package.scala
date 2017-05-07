package mts.experiments

import nlpdata.datasets.conll.CoNLLSentencePath
import nlpdata.datasets.conll.CoNLLSentence

package object sample extends PackagePlatformExtensions {
  case class SamplePrompt(path: CoNLLSentencePath)
  case class SampleResponse(isGood: Boolean)

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(path: CoNLLSentencePath, sentence: CoNLLSentence) extends ApiResponse
}
