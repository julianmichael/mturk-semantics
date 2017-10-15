package example.emnlp2017.silly

import nlpdata.datasets.wiktionary.InflectedForms

case class InflectionalWord(
  token: String,
  pos: String,
  index: Int,
  inflectedFormsOpt: Option[InflectedForms])
