package mts

import mts.language.TextRendering

package object conll extends PackagePlatformExtensions {
  implicit class CoNLLTextRendering(val tr: TextRendering.type) extends AnyVal {
    def renderSentence(sentence: CoNLLSentence) = {
      tr.renderSentence(sentence.words.map(_.token))
    }
  }
}
