package mts.experiments

import mts.datasets.conll.CoNLLSentencePath
import mts.datasets.conll.CoNLLSentence
import mts.datasets.conll.Word

package object expE extends PackagePlatformExtensions {
  case class QAGenPrompt(path: CoNLLSentencePath, wordIndex: Int)
  case class QAGenResponse(qaPairs: List[(String, Set[Int])])

  def isQAPairNonempty(qaPair: (String, Set[Int])): Boolean =
    !qaPair._1.isEmpty && !qaPair._2.isEmpty

  def spanTokens(sentence: CoNLLSentence, span: Set[Int]): List[String] =
    sentence.words.filter(w => span.contains(w.index)).map(_.token)

  def renderSpan(sentence: CoNLLSentence, span: Set[Int]): String =
    mts.language.TextRendering.renderSentence(spanTokens(sentence, span))

  def printableWord(sentence: CoNLLSentence, index: Int) = s"${sentence.words(index).index}:${sentence.words(index).token}"
  def printableWord(word: Word) = s"${word.index}:${word.token}"

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(path: CoNLLSentencePath, sentence: CoNLLSentence) extends ApiResponse

  val bonuses = List(0.0, 0.06, 0.08, 0.10, 0.12)
  val numQAs = bonuses.size
}
