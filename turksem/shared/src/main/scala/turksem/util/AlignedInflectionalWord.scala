package turksem.util

import nlpdata.structure.AlignedToken
import nlpdata.datasets.wiktionary.InflectedForms

case class AlignedInflectionalWord(
  token: String,
  originalText: String,
  whitespaceBefore: String,
  whitespaceAfter: String,
  pos: String,
  index: Int,
  inflectedFormsOpt: Option[InflectedForms]) {

  def outOfContextToken =
    if(index == 0 && token.head.isUpper && !pos.startsWith("NNP")) {
      (token.head.toLower + token.tail)
    } else token

  def alignedToken = AlignedToken(
    token = token,
    originalText = originalText,
    whitespaceBefore = whitespaceBefore,
    whitespaceAfter = whitespaceAfter)
}
