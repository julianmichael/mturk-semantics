package mts.experiments

import mts.conll.CoNLLSentencePath
import mts.conll.CoNLLSentence

package object sample {
  case class SamplePrompt(path: CoNLLSentencePath)
  case class SampleResponse(isGood: Boolean)

  trait SampleApi {
    def getCoNLLSentence(path: CoNLLSentencePath): CoNLLSentence
  }
}
