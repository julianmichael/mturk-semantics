package turksem.iqa

import cats.Show
import cats.implicits._
import nlpdata.util.LowerCaseStrings._

sealed trait Token {
  def isSlot = this match {
    case StringToken(_) => false
    case Copula => false
    case AlignedVerb(_) => true
    case _: Argument => true
  }
}
object Token {
  implicit val tokenShow = new Show[Token] {
    override def show(t: Token) = t match {
      case s: StringToken => s.show
      case mv: MainVerb => mv.show
      case a: Argument => a.show
    }
  }
}

case class StringToken(string: LowerCaseString) extends Token
object StringToken {
  implicit val stringTokenShow: Show[StringToken] = new Show[StringToken] {
    override def show(s: StringToken) = s.string.toString
  }
}

sealed trait MainVerb extends Token
object MainVerb {
  implicit val mainVerbShow: Show[MainVerb] = new Show[MainVerb] {
    import VerbVoice._
    override def show(mv: MainVerb) = mv match {
      case Copula => "<is>"
      case AlignedVerb(Active) => "V"
      case AlignedVerb(Passive) => "V-pss"
    }
  }
}
case object Copula extends MainVerb

sealed trait Argument extends Token

case class AlignedVerb(voice: VerbVoice) extends MainVerb with Argument
object AlignedVerb {
  import VerbVoice._
  val act = AlignedVerb(Active)
  val pss = AlignedVerb(Passive)
}

object Argument {
  implicit val argumentShow: Show[Argument] = new Show[Argument] {
    import NounConstraint._
    import VerbVoice._
    import AdjectiveKind._
    override def show(a: Argument) = a match {
      case AlignedVerb(Active) => "V"
      case AlignedVerb(Passive) => "V-pss"
      case Noun(Bare) => "N"
      case Noun(Plural) => "N-pl"
      case Noun(Proper) => "N-prop"
      case Noun(DetPhrase) => "N-det"
      case Noun(DetOrVerb) => "N/V"
      case Adjective(Regular) => "J"
      case Adjective(Comparative) => "J-cmp"
      case Adjective(Superlative) => "J-sup"
      case ToVerb => "V-to"
      case Preposition => "P"
    }
  }
}
case object Preposition extends Argument
case class Adjective(kind: AdjectiveKind) extends Argument
object Adjective {
  import AdjectiveKind._
  val reg = Adjective(Regular)
  val cmp = Adjective(Comparative)
  val sup = Adjective(Superlative)
}

sealed trait GappableArgument extends Argument {
  import NounConstraint._
  def placeholder: String = this match {
    case Noun(Bare) => "thing"
    case Noun(Plural) => "things"
    case Noun(Proper) => "someone"
    case Noun(DetPhrase) => "something"
    case Noun(DetOrVerb) => "something"
    case ToVerb => "to do something"
  }
  def gap: List[StringToken] = this match {
    case ToVerb => List(StringToken("to".lowerCase), StringToken("do".lowerCase))
    case Noun(_) => Nil
  }
}
case class Noun(constraint: NounConstraint) extends GappableArgument
object Noun {
  import NounConstraint._
  val bare = Noun(Bare)
  val pl = Noun(Plural)
  val prop = Noun(Proper)
  val det = Noun(DetPhrase)
  val dg = Noun(DetOrVerb)
}
case object ToVerb extends GappableArgument

sealed trait AdjectiveKind {
  import AdjectiveKind._
  final def pos = this match {
    case Regular => "JJ"
    case Comparative => "JJR"
    case Superlative => "JJS"
  }
}
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
  case object DetOrVerb extends NounConstraint
  // case object Genitive extends NounConstraint
}
