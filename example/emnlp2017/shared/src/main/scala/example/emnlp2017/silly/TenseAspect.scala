package example.emnlp2017.silly

import nlpdata.util.LowerCaseStrings._

sealed trait Tense
case class Modal(modalVerb: LowerCaseString) extends Tense
case object PresentTense extends Tense
case object PastTense extends Tense

case class TenseAspect(
  tense: Tense,
  isPerfect: Boolean,
  isProgressive: Boolean,
  isNegated: Boolean)
