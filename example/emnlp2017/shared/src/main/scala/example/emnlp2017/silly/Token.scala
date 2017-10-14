package example.emnlp2017.silly

import nlpdata.util.LowerCaseStrings._

sealed trait Token

case class StringToken(string: LowerCaseString) extends Token

sealed trait MainVerb extends Token
case object Copula extends MainVerb
case class AlignedVerb(voice: VerbVoice) extends MainVerb
object AlignedVerb {
  import VerbVoice._
  val act = AlignedVerb(Active)
  val pss = AlignedVerb(Passive)
}

sealed trait Argument extends Token
case class Noun(constraint: NounConstraint) extends Argument
object Noun {
  import NounConstraint._
  val bare = Noun(Bare)
  val pl = Noun(Plural)
  val prop = Noun(Proper)
  val det = Noun(DetPhrase)
  val dg = Noun(DetOrGerund)
}
case object ToVerb extends Argument
case object Preposition extends Argument
case class Adjective(kind: AdjectiveKind) extends Argument
object Adjective {
  import AdjectiveKind._
  val reg = Adjective(Regular)
  val cmp = Adjective(Comparative)
  val sup = Adjective(Superlative)
}

sealed trait AdjectiveKind
object AdjectiveKind {
  case object Regular extends AdjectiveKind
  case object Comparative extends AdjectiveKind
  case object Superlative extends AdjectiveKind
}

sealed trait VerbVoice
object VerbVoice {
  case object Active extends VerbVoice
  case object Passive extends VerbVoice
}

sealed trait NounConstraint
object NounConstraint {
  case object Bare extends NounConstraint
  case object Plural extends NounConstraint
  case object Proper extends NounConstraint
  case object DetPhrase extends NounConstraint
  case object DetOrGerund extends NounConstraint
  // case object Genitive extends NounConstraint
}
