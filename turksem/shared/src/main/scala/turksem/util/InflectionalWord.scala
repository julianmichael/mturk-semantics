package turksem.util

import nlpdata.datasets.wiktionary.InflectedForms

case class InflectionalWord(
  token: String,
  pos: String,
  index: Int,
  inflectedFormsOpt: Option[InflectedForms]) {
  def outOfContextToken =
    if(index == 0 && token.head.isUpper && !pos.startsWith("NNP")) {
      (token.head.toLower + token.tail)
    } else token
}
