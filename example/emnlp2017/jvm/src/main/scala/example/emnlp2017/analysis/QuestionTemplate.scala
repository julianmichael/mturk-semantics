package example.emnlp2017.analysis

import turksem.qamr._
import example.emnlp2017._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

sealed trait TemplateToken[+Slot] {
  def isTemplateSlot: Boolean = this match {
    case TemplateSlot(_) => true
    case TemplateString(_) => false
  }
  def isTemplateString: Boolean = !isTemplateSlot
}
case class TemplateString(value: LowerCaseString) extends TemplateToken[Nothing]
case class TemplateSlot[+Slot](s: Slot) extends TemplateToken[Slot]
object TemplateToken {
  def templateString[Slot](value: LowerCaseString): TemplateToken[Slot] = TemplateString(value)
  def templateSlot[Slot](s: Slot): TemplateToken[Slot] = TemplateSlot(s)
}

// can use Slot to represent different kinds of things we abstract out

case class QuestionTemplate[+Slot](templateTokens: List[TemplateToken[Slot]]) {
  def getTokensLower(implicit ev: Slot <:< Vector[LowerCaseString]) = templateTokens.flatMap {
    case TemplateSlot(ss) => ss
    case TemplateString(s) => Vector(s)
  }.toVector

  def getTokens(implicit ev: Slot <:< Vector[String]) = templateTokens.flatMap {
    case TemplateSlot(ss) => ss
    case TemplateString(s) => Vector(s)
  }.toVector

  def fillSpansA[F[_]: Applicative](
    renderSlot: Slot => F[Vector[LowerCaseString]]
  ): F[Vector[LowerCaseString]] =
    this.traverse(renderSlot).map(_.getTokensLower)

  def fillSpans(
    renderSlot: Slot => Vector[LowerCaseString]
  ): Vector[LowerCaseString] =
    fillSpansA[Id](renderSlot)

  def fillSpansId(
    id: SentenceId)(
    implicit ev: Slot <:< ContiguousSpan
  ): Vector[LowerCaseString] =
    fillSpans((span: Slot) => span.getTokens(id).map(_.lowerCase))

  def substituteSlots[Arg, Out](
    arguments: List[Arg],
    substitute: (Slot, Arg) => Out): QuestionTemplate[Out] = {
    this.traverse[λ[A => State[List[Arg], A]], Out] { in =>
      import State._
      for {
        rest <- get[List[Arg]]
        _ <- set(rest.tail)
      } yield substitute(in, rest.head)
    }.runA(arguments).value
  }

  def replaceSlots[Out](
    outs: List[Out]
  ): QuestionTemplate[Out] =
    substituteSlots(outs, (x: Slot, y: Out) => y)

  def zipSlots[Arg, Out](
    arguments: List[Arg]
  ): QuestionTemplate[(Slot, Arg)] = {
    substituteSlots(arguments, (i: Slot, a: Arg) => (i, a))
  }

}
object QuestionTemplate {
  import TemplateToken._

  implicit def questionTemplateShow[A : Show] = Show.show((qt: QuestionTemplate[A]) =>
    qt.map(a => Vector(a.show)).getTokens.mkString(" ")
  )

  implicit def questionTemplateOrder[A : Show]: Order[QuestionTemplate[A]] = Order.by[QuestionTemplate[A], String](_.show)

  implicit val questionTemplateTraverse: Traverse[QuestionTemplate] = new Traverse[QuestionTemplate] {
    def traverse[G[_]: Applicative, A, B](
      fa: QuestionTemplate[A])(
      f: A => G[B]
    ): G[QuestionTemplate[B]] = {
      val newTokens = fa.templateTokens.traverse { token =>
        token match {
          case TemplateString(s) => Applicative[G].pure(templateString[B](s))
          case TemplateSlot(in) => f(in).map(templateSlot(_))
        }
      }
      newTokens.map(QuestionTemplate(_))
    }

    def foldLeft[A, B](fa: QuestionTemplate[A], b: B)(f: (B, A) => B): B =
      fa.templateTokens.foldLeft(b) {
        case (acc, TemplateString(_)) => acc
        case (acc, TemplateSlot(slot)) => f(acc, slot)
      }

    def foldRight[A, B](fa: QuestionTemplate[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.templateTokens.foldRight(lb) {
        case (TemplateString(_), acc) => acc
        case (TemplateSlot(slot), acc) => f(slot, acc)
      }
  }
}

sealed trait Reinflection
case object NoReinflection extends Reinflection
case class VerbReinflection(form: Int) extends Reinflection
case class NounReinflection(form: Int) extends Reinflection

object Reinflection {

  val noReinflection: Reinflection = NoReinflection
  def verbReinflection(form: Int): Reinflection = VerbReinflection(form)
  def nounReinflection(form: Int): Reinflection = NounReinflection(form)

  implicit val reinflectionShow: Show[Reinflection] = Show.show { reinflection =>
    reinflection match {
      case NoReinflection => "_"
      case VerbReinflection(form) => form match {
        case 0 => s"_-vstem" // base/inf
        case 1 => s"_-s(v)" // present
        case 2 => s"_-ing(v)" // present participle
        case 3 => s"_-ed(v)" // past
        case 4 => s"_-en(v)" // past participle
      }
      case NounReinflection(form) => form match {
        case 0 => s"_-nstem"
        case 1 => s"_-s(n)"
        case 2 => s"_-ing(n)"
        case 3 => s"_-ed(n)"
        case 4 => s"_-en(n)"
      }
    }
  }
}

// QuestionTemplateAlignment is specific to a sentence and question about that sentence.

case class QuestionTemplateAlignment[Slot](
  sourcedQA: SourcedQA[SentenceId],
  template: QuestionTemplate[Slot],
  alignments: List[List[ContiguousSpan]]) {
  val answers = sourcedQA.answers.map(indices => ContiguousSpan(indices.min, indices.max + 1))
  def sentenceId = sourcedQA.id.sentenceId
  def question = sourcedQA.question

  if(template.size != alignments.size) {
    println("=== Template v alignment size disagreement ===")
    println(Text.render(sourcedQA.id.sentenceId))
    println(sourcedQA)
    println(template)
    println(alignments)
  }
  val templateWithAlignmentLists = template.replaceSlots(alignments)

  def matches(iq: QuestionTemplate[ContiguousSpan]): Boolean =
    templateWithAlignmentLists.templateTokens.size == iq.templateTokens.size &&
      (templateWithAlignmentLists.templateTokens, iq.templateTokens).zipped.forall {
        case (TemplateString(s), TemplateString(t)) => s == t
        case (TemplateSlot(refSpans), TemplateSlot(span)) => refSpans.contains(span)
        case _ => false
      }
}
