package mts.experiments

import mts.conll.CoNLLSentencePath

package object expD {
  case class WordChoosingPrompt(path: CoNLLSentencePath)
  case class WordChoosingResponse(items: List[WordChoosingResponseItem])
  case class WordChoosingResponseItem(
    questionWord: String,
    answerWord: String,
    question: String,
    answer: String
  )
}
