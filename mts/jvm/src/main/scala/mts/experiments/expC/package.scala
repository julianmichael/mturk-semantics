package mts.experiments

import mts.conll.CoNLLSentencePath

package object expC {
  case class TabooAnswersPrompt(
    path: CoNLLSentencePath,
    tabooList: List[String])

  case class TabooAnswersResponse(
    qaPairs: List[(String, String)])
}
