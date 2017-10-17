package turksem.iqa

import cats.Show
import cats.implicits._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

case class Template(tokens: List[Token]) {
  def size = tokens.filter(_.isSlot).size
  def arguments = tokens.collect { case a: Argument => a }
  def numArguments = arguments.size
  def mainVerb = tokens.collect { case m: MainVerb => m }.head
  def gappableArgumentIndices = arguments.zipWithIndex.collect {
    case (_: GappableArgument, i) => i
  }

  def makeAlignedQATemplate(targetIndex: Int): AlignedQATemplate = {
    val targetTokenIndex = tokens.zipWithIndex.collect {
      case (a: Argument, i) => i
    }.apply(targetIndex)
    val aux = mainVerb match {
      case AlignedVerb(_) => StringToken("<aux>".lowerCase)
      case Copula => Copula
    }
    val gappedArg = arguments(targetIndex).asInstanceOf[GappableArgument]
    val tokensWithNoTarget = tokens.take(targetTokenIndex) ++ gappedArg.gap ++ tokens.drop(targetTokenIndex + 1)
    val tokensWithNoTargetAndMaybeAux = tokensWithNoTarget.head match {
      case AlignedVerb(_) => tokensWithNoTarget
      case _ => aux :: tokensWithNoTarget.filter(_ != Copula)
    }
    AlignedQATemplate(
      QATemplate(
        StringToken("what".lowerCase) :: tokensWithNoTargetAndMaybeAux,
        gappedArg
      ),
      this,
      (arguments.indices.take(targetIndex) ++ arguments.indices.drop(targetIndex + 1)).toVector,
      targetIndex
    )
  }

  def makeAllAlignedQATemplates: List[AlignedQATemplate] =
    gappableArgumentIndices.map(makeAlignedQATemplate)
}
object Template {
  implicit val templateShow: Show[Template] = new Show[Template] {
    override def show(t: Template) = t.tokens.map(_.show).mkString(" ")
  }
}

case class QATemplate(
  questionTokens: List[Token],
  answer: GappableArgument) {
  def questionArguments = questionTokens.collect {
    case a: Argument => a
  }
}

case class AlignedQATemplate(
  qaTemplate: QATemplate,
  propositionTemplate: Template,
  questionAlignments: Vector[Int],
  answerAlignment: Int
)
