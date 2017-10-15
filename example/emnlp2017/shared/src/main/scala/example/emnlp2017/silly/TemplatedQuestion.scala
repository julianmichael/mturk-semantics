package example.emnlp2017.silly

import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

// prepositions should always appear as "external words"

sealed trait ArgumentSpecifier
case class SentenceWord(index: Int) extends ArgumentSpecifier
case class ExternalWord(word: LowerCaseString) extends ArgumentSpecifier
case class Entity(questionIndex: Int) extends ArgumentSpecifier

case class TemplatedQuestion(
  template: Template,
  targetIndex: Int,
  tenseAspect: TenseAspect,
  arguments: List[ArgumentSpecifier]) {

  def argsWithAnswerPlaceholder: List[Option[ArgumentSpecifier]] = arguments.zipWithIndex.flatMap {
    case (arg, `targetIndex`) => List(None, Some(arg))
    case (arg, _) => List(Some(arg))
  }

  def reconstructedTemplateArgs(answerArgument: ArgumentSpecifier) =
    argsWithAnswerPlaceholder.map(_.getOrElse(answerArgument))

  def renderQuestion(
    sentence: Vector[InflectionalWord],
    entityStrings: Vector[String]
  ): String = {
    // TODO track number/person of subject to make copula agree
    case class TokenRenderingState(
      nextArgIndex: Int = 0,
      tokens: List[String] = Nil,
      argumentsRemaining: List[ArgumentSpecifier] = arguments)
    val tokenRenderingState = template.tokens.foldLeft(TokenRenderingState()) {
      case (st, StringToken(s)) => st.copy(tokens = st.tokens ++ List(s.toString))
      case (st, mv: MainVerb) =>
        val (verbRenderer, intermediateState) = mv match {
          case Copula =>
            (new TenseAspect.VerbRenderer(
               InflectedForms.beSingularForms, tenseAspect, VerbVoice.Active), st)
          case AlignedVerb(voice) => st.argumentsRemaining match {
            case SentenceWord(verbIndex) :: args =>
              (new TenseAspect.VerbRenderer(
                 sentence(verbIndex).inflectedFormsOpt.get, tenseAspect, voice),
               st.copy(
                 nextArgIndex = st.nextArgIndex + 1,
                 argumentsRemaining = args))
            case _ => ??? // should never happen
          }
        }
        if(intermediateState.tokens.isEmpty) { // no need to split aux
          intermediateState.copy(
            tokens = verbRenderer.getVerbTokens.toList ++ intermediateState.tokens
          )
        } else { // split aux
          val verbTokens = verbRenderer.splitIfNecessary(verbRenderer.getVerbTokens)
          intermediateState.copy(
            tokens = verbTokens.head :: intermediateState.tokens ++ verbTokens.tail
          )
        }
      case (st, a: GappableArgument) if st.nextArgIndex == targetIndex =>
        st.copy(
          nextArgIndex = st.nextArgIndex + 1,
          tokens = a.gap.fold(st.tokens)(st.tokens ++ List(_)))
      case (st, a: Argument) => st.argumentsRemaining match {
        case Nil => ??? // should never happen
        case arg :: args =>
          val argToken = arg match {
            // TODO fix capitalization for sentence word, possibly reinflect
            case SentenceWord(index) => sentence(index).token
            case ExternalWord(word) => word.toString
            case Entity(index) => entityStrings(index)
          }
          st.copy(
            nextArgIndex = st.nextArgIndex + 1,
            tokens = st.tokens ++ List(argToken),
            argumentsRemaining = args)
      }
      case _ => ??? // should never happen
    }
    Text.render("what" :: tokenRenderingState.tokens).capitalize + "?"
  }
}
