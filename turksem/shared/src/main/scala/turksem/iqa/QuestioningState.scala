package turksem.iqa

import turksem.util.ContiguousSpan

import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import monocle.macros.Lenses
import monocle.macros.GenPrism

@Lenses case class QuestioningState(
  sentence: Vector[InflectionalWord],
  qas: Vector[TemplatedQA]
) {

  val entityStrings = qas.zipWithIndex.flatMap {
    case (qa, index) => qa.judgment.getAnswer.map(a => index -> a.standin)
  }.toMap

  // TODO more general for fancy question stuff. maybe need to make changes in TemplatedQuestion
  def renderQuestion(index: Int): Option[String] = {
    qas.lift(index).map(_.question.renderQuestion(sentence, entityStrings))
  }

  lazy val triggerGroups = sentence.flatMap { w =>
    templates.getGroupForPosTag(w.pos).map { triggerTemplates =>
      val qasForTrigger = qas.zipWithIndex.filter { case (tqa, _) =>
        templates.getTriggerIndex(tqa.question.template).fold(false) { triggerIndex =>
          tqa.question.argsWithAnswerPlaceholder(triggerIndex) match {
            case Some(SentenceWord(i)) if i == w.index => true
            case _ => false
          }
        }
      }.map { case (tqa, index) => index -> tqa }.toMap

      TriggerGroup(w, triggerTemplates, qasForTrigger)
    }
  }
}

case class TriggerGroup(
  trigger: InflectionalWord,
  templates: List[(Template, Int)],
  qas: Map[Int, TemplatedQA]) { // map from original index
                                // TODO calculate alignments
}

@Lenses case class TemplatedQA(
  question: TemplatedQuestion,
  judgment: QuestionJudgment)

sealed trait QuestionJudgment {
  def getAnswer = this match {
    case a @ Answer(_, _) => Some(a)
    case _ => None
  }
  def isAnswer = getAnswer.nonEmpty

  def getNoAnswer = this match {
    case NoAnswer => Some(NoAnswer)
    case _ => None
  }
  def isNoAnswer = getNoAnswer.nonEmpty

  def getBadQuestion = this match {
    case BadQuestion => Some(BadQuestion)
    case _ => None
  }
  def isBadQuestion = getBadQuestion.nonEmpty
}
object QuestionJudgment {
  def answer = GenPrism[QuestionJudgment, Answer]
  def noAnswer = GenPrism[QuestionJudgment, NoAnswer.type]
  def badQuestion = GenPrism[QuestionJudgment, BadQuestion.type]
}

@Lenses case class Answer(
  spans: List[ContiguousSpan],
  standin: String
) extends QuestionJudgment
case object NoAnswer extends QuestionJudgment
case object BadQuestion extends QuestionJudgment

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
    entityStrings: Map[Int, String]
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
            case Entity(index) => entityStrings.get(index).getOrElse("something")
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

// prepositions should always appear as "external words"

sealed trait ArgumentSpecifier
case class SentenceWord(index: Int) extends ArgumentSpecifier
case class ExternalWord(word: LowerCaseString) extends ArgumentSpecifier
case class Entity(questionIndex: Int) extends ArgumentSpecifier