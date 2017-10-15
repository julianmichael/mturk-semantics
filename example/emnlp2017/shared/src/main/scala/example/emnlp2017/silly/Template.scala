package example.emnlp2017.silly

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
  def gappableIndices = tokens.zipWithIndex.collect {
    case (_: GappableArgument, i) => i
  }

  // private val takeForms = InflectedForms(
  //   stem = "take".lowerCase,
  //   present = "takes".lowerCase,
  //   presentParticiple = "taking".lowerCase,
  //   past = "took".lowerCase,
  //   pastParticiple = "taken".lowerCase)

  // def testInstantiate(ta: TenseAspect, targetIndex: Int) = {
  //   val arguments = tokens
  //     .collect { case a: Argument => a.show }
  //     .zipWithIndex
  //     .filter(_._2 != targetIndex)
  //     .map { case (a, i) => List(s"$a-$i") }
  //   if(mainVerb == Copula) instantiate(ta, arguments, targetIndex, None)
  //   else instantiate(ta, arguments, targetIndex, Some(takeForms))
  // }

  // def instantiate(
  //   ta: TenseAspect,
  //   arguments: List[List[String]],
  //   targetIndex: Int,
  //   verbInflectedForms: Option[InflectedForms]
  // ): String = {
  //   // TODO track number/person of subject to make copula agree
  //   case class TokenRenderingState(
  //     nextArgIndex: Int = 0,
  //     tokens: List[String] = Nil,
  //     argumentsRemaining: List[List[String]] = arguments)
  //   val tokenRenderingState = tokens.foldLeft(TokenRenderingState()) {
  //     case (st, StringToken(s)) => st.copy(tokens = st.tokens ++ List(s.toString))
  //     case (st, mv: MainVerb) =>
  //       val verbRenderer = mv match {
  //         case Copula => new TenseAspect.VerbRenderer(
  //           InflectedForms.beSingularForms, ta, VerbVoice.Active)
  //         case AlignedVerb(voice) => new TenseAspect.VerbRenderer(
  //           verbInflectedForms.get, ta, voice)
  //       }
  //       if(st.tokens.isEmpty) { // no need to split aux
  //         st.copy(tokens = verbRenderer.getVerbTokens.toList ++ st.tokens)
  //       } else { // split aux
  //         val verbTokens = verbRenderer.splitIfNecessary(verbRenderer.getVerbTokens)
  //         st.copy(tokens = verbTokens.head :: st.tokens ++ verbTokens.tail)
  //       }
  //     case (st, a: GappableArgument) if st.nextArgIndex == targetIndex =>
  //       st.copy(
  //         nextArgIndex = st.nextArgIndex + 1,
  //         tokens = a.gap.fold(st.tokens)(st.tokens ++ List(_)))
  //     case (st, a: Argument) => st.argumentsRemaining match {
  //       case Nil => ??? // should never happen
  //       case arg :: args => st.copy(
  //         nextArgIndex = st.nextArgIndex + 1,
  //         tokens = st.tokens ++ arg,
  //         argumentsRemaining = args)
  //     }
  //     case _ => ??? // should never happen
  //   }
  //   Text.render("what" :: tokenRenderingState.tokens).capitalize + "?"
  // }
}
object Template {
  implicit val templateShow: Show[Template] = new Show[Template] {
    override def show(t: Template) = t.tokens.map(_.show).mkString(" ")
  }
}
