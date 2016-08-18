package mts.experiments

import mts.conll.CoNLLSentencePath

package object expA {
  type OpenFormPrompt = (CoNLLSentencePath, String) // path to sentence, sentence
  type OpenFormResponse = (List[(String, String)], String) // QA pairs, comment
}
