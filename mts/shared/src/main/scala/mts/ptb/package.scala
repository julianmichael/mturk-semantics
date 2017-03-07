package mts

import mts.language.TextRendering

package object ptb extends PackagePlatformExtensions {
  implicit class PTBTextRendering(val tr: TextRendering.type) extends AnyVal {
    def getTokens(sentence: PTBSentence): Vector[String] = {
      sentence.words.filter(_.pos != "-NONE-").map(_.token)
    }

    def renderSentence(sentence: PTBSentence) = {
      tr.renderSentence(getTokens(sentence))
    }

    def renderSpan(sentence: PTBSentence, span: Set[Int]) = {
      tr.renderSentence(getTokens(sentence).zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
    }
  }
}
