package turksem.gapfill

import cats.data.State
import cats.data.NonEmptyList
import cats.implicits._

import monocle.macros._
import turksem.util._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags
import nlpdata.util.Text

import turksem.iqa.TenseAspect
import turksem.iqa.VerbVoice
import TenseAspect.VerbRenderer

import monocle.function.{all => Optics}

sealed trait TemplateArgument
case class PreviousAnswerSpan(triggerIndex: Int, questionIndex: Int) extends TemplateArgument
case class SentenceWord(indexInSentence: Int) extends TemplateArgument

@Lenses case class InstantiatedQuestion(
  template: QuestionTemplate[TriggerSlot],
  arguments: List[TemplateArgument])

sealed trait QuestionJudgment {
  def getBadQuestion = this match {
    case BadQuestion => Some(BadQuestion)
    case _ => None
  }
  def isBadQuestion = getBadQuestion.nonEmpty

  def getAnswer = this match {
    case a @ Answer(_) => Some(a)
    case _ => None
  }
  def isAnswer = getAnswer.nonEmpty
}
case object BadQuestion extends QuestionJudgment
@Lenses case class Answer(span: Set[Int]) extends QuestionJudgment
object QuestionJudgment {
  val answer = GenPrism[QuestionJudgment, Answer]
  val badQuestion = GenPrism[QuestionJudgment, BadQuestion.type]
}

@Lenses case class JudgedQuestion(
  question: InstantiatedQuestion,
  judgment: QuestionJudgment)

@Lenses case class QuestioningState(
  sentence: Vector[InflectionalWord],
  triggerGroups: Vector[TriggerGroup]) {

  case class ContentVerbSpec(
    stem: LowerCaseString,
    isPassive: Boolean
  )

  def renderQuestion(iq: InstantiatedQuestion): String = iq match {
    case InstantiatedQuestion(template, arguments) =>
      val tenseAspect = TenseAspect.singularPresent.copy(isProgressive = true)
      val potentialVerbRenderers = template.zipWithIndex.templateTokens.zipWithIndex.flatMap {
        case (templateToken, tokenIndex) =>
          val rendererOpt: Option[VerbRenderer] = templateToken match {
            case TemplateSlot((TriggerSlot(label, _), argIndex)) if label.startsWith("VERB") =>
              arguments(argIndex) match {
                case SentenceWord(i) =>
                  sentence(i).inflectedFormsOpt.map(inflForms =>
                    new VerbRenderer(
                      inflForms, tenseAspect,
                      if(label.endsWith("pss")) VerbVoice.Passive else VerbVoice.Active)
                  )
                case PreviousAnswerSpan(gIndex, qIndex) =>
                  triggerGroups(gIndex).qas(qIndex)
                    .judgment.getAnswer.map(_.span).filter(_.nonEmpty).fold(
                    Option( // TODO do I really want to do "do" when the verb is missing? ehhhhhh
                      new VerbRenderer(
                        InflectedForms.doForms, tenseAspect, VerbVoice.Active
                      )
                    )
                  )(span =>
                    (span.min to span.max).map(sentence.apply).collect {
                      case sw if PosTags.verbPosTags.contains(sw.pos) => sw.inflectedFormsOpt.map(inflForms =>
                        new VerbRenderer(
                          inflForms, tenseAspect, VerbVoice.Active
                        )
                      )
                    }.flatten.headOption
                  )
              }
            case TemplateString(s) =>
              def makeResult(inflForms: InflectedForms) = Option(new VerbRenderer(inflForms, tenseAspect, VerbVoice.Active))
              if(s == "<be>".lowerCase) makeResult(InflectedForms.beSingularForms)
              else if(s == "<do>".lowerCase) makeResult(InflectedForms.doForms)
              else None
            case _ => None
          }
          rendererOpt.map(_ -> tokenIndex)
      }
      if(potentialVerbRenderers.size > 1) {
        println("More than one potential verb: " + template.show)
      }
      val verbRendererOpt = potentialVerbRenderers.headOption
      val verbStackOpt = verbRendererOpt.map(p => p._1.getVerbTokens -> p._2)
      type S = Option[(NonEmptyList[String], Int)]
      val vecState = template.zipWithList(arguments)
        .map { case (slot, argOpt) => (slot, argOpt.get) }
        .templateTokens.toVector.zipWithIndex.traverse[State[S, ?], Vector[String]] {
        case (templateToken, tokenIndex) => templateToken match {
          case TemplateSlot((_, PreviousAnswerSpan(triggerIndex, questionIndex))) =>
            State.pure[S, Vector[String]](
              triggerGroups(triggerIndex).qas(questionIndex).judgment match {
                case BadQuestion => Vector("something") // shouldn't happen?
                case Answer(span) => Vector(Text.renderSpan(sentence.map(_.outOfContextToken), span))
              }
            )
          case TemplateSlot((TriggerSlot(label, _), SentenceWord(indexInSentence))) =>
            State.get[S].map { s =>
              s.filter(_._2 == tokenIndex)
                .fold(Vector(sentence(indexInSentence).outOfContextToken))(_._1.toList.toVector)
            }
          case TemplateString(s) => s.toString match {
            case "<be>" => State.get[S].map { s =>
              s.filter(_._2 == tokenIndex)
                .fold(Vector("be"))(_._1.toList.toVector)
            }
            case "<do>" => State.get[S].map { s =>
              s.filter(_._2 == tokenIndex)
                .fold(Vector("do"))(_._1.toList.toVector)
            }
            case "<aux>" => State.get[S].flatMap { s =>
              s.fold(State.pure[S, Vector[String]](Vector.empty[String])) { case (verbStack, verbIndex) =>
                // since it's present we know the renderer is present too
                val newStack = verbRendererOpt.get._1.splitIfNecessary(verbStack)
                State.set(Option(NonEmptyList.fromList(newStack.tail).get -> verbIndex))
                  .as(Vector(newStack.head))
              }
            }
            case s => State.pure[S, Vector[String]](
              s match {
                case "<who/what>" => Vector("what")
                case "<what kind of>" => Vector("what", "kind", "of")
                case _ => Vector(s)
              }
            )
          }
        }
      }
      Text.render(vecState.runA(verbStackOpt).value.flatten).capitalize
  }
}
object QuestioningState {
  // ignores passiveness of verbs
  def getLabelForPos(pos: String) = {
    val prelimLabel = pos match {
      case "NN" => "NOUN"
      case "NNS" => "NOUN-pl"
      case "NNP" => "NOUN-prop"
      case "NNPS" => "NOUN-prop-pl"
      case v if PosTags.verbPosTags.contains(v) => "VERB"
      case "JJ" => "ADJ"
      case "JJR" => "ADJ-cmp"
      case "JJS" => "ADJ-sup"
      case _ => "bah"
    }
    prelimLabel match {
      case "bah" => None
      case x => Some(x)
    }
  }


  def initFromSentence(sentence: Vector[InflectionalWord]) = {
    QuestioningState(
      sentence,
      for {
        w <- sentence
        if w.inflectedFormsOpt.nonEmpty
        label <- getLabelForPos(w.pos)
      } yield TriggerGroup(w, label, Vector.empty[JudgedQuestion])
    )
  }
  def empty = QuestioningState(Vector(), Vector())
}

@Lenses case class TriggerGroup(
  trigger: InflectionalWord,
  triggerLabel: String,
  qas: Vector[JudgedQuestion]) {
}
