package mts

import mts.language.TextRendering

package object ptb extends PackagePlatformExtensions {
  implicit class PTBTextRendering(val tr: TextRendering.type) extends AnyVal {
    def renderSentence(sentence: PTBSentence) = {
      tr.renderSentence(sentence.words.filter(_.pos != "-NONE-").map(_.token))
    }
  }
}
