package turksem.qasrl

import turkey.tasks._
import cats.implicits._

import turksem._
import turksem.qamr._
import turksem.util._

import nlpdata.util.Text

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

class QASRLValidationClient[SID : Writer : Reader](
  instructions: VdomTag)(
  implicit promptReader: Reader[QASRLValidationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[QASRLValidationAnswer]] // same as above
) extends TaskClient[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]] {

  import QASRLSettings._

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  case class AnswerWordIndex(
    questionIndex: Int,
    answerIndex: Int,
    wordIndex: Int)
  object AnswerWordIndex {
    def getAnswerSpans(indices: Set[AnswerWordIndex]): Map[Int, Answer] =
      prompt.qaPairs.indices
        .map(qi =>
        qi -> Answer(
          indices
            .filter(_.questionIndex == qi)
            .groupBy(_.answerIndex)
            .toList
            .sortBy(_._1)
            .map(_._2.map(awi => awi.wordIndex).toSet))
      ).toMap
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[QASRLValidationApiRequest[SID], QASRLValidationApiResponse]
  import WebsocketLoadableComponent._
  val SpanHighlightingComponent = new SpanHighlightingComponent[Int] // question
  import SpanHighlightingComponent._

  import MultiContigSpanHighlightableSentenceComponent._

  lazy val questions = prompt.qaPairs.map(_.question)

  @Lenses case class State(
    curQuestion: Int,
    isInterfaceFocused: Boolean,
    answers: List[QASRLValidationAnswer])
  object State {
    def initial = State(0, false, questions.map(_ => Answer(List.empty[Set[Int]])))
  }

  def answerSpanOptics(questionIndex: Int) =
    State.answers
      .composeOptional(Optics.index(questionIndex))
      .composePrism(QASRLValidationAnswer.answer)
      .composeLens(Answer.spans)

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse: Callback = scope.state.map { state =>
      setResponse(state.answers)
    }

    def updateCurrentAnswers(highlightingState: SpanHighlightingState) =
      scope.state >>= (st =>
        scope.modState(
          answerSpanOptics(st.curQuestion).set(
            highlightingState.spans(st.curQuestion).map(_.indices)
          )
        )
      )

    def toggleInvalidAtIndex(highlightedAnswers: Map[Int, Answer])(questionIndex: Int) =
      scope.modState(
        State.answers.modify(answers =>
          answers.updated(
            questionIndex,
            if(answers(questionIndex).isInvalid) highlightedAnswers(questionIndex)
            else InvalidQuestion)
        )
      )

    def handleKey(highlightedAnswers: Map[Int, Answer])(e: ReactKeyboardEvent): Callback = {
      def nextQuestion = scope.modState(State.curQuestion.modify(i => (i + 1) % questions.size))
      def prevQuestion = scope.modState(State.curQuestion.modify(i => (i + questions.size - 1) % questions.size))
      def toggleInvalid = scope.zoomStateL(State.curQuestion).state >>= toggleInvalidAtIndex(highlightedAnswers)

      if(isNotAssigned) {
        Callback.empty
      } else CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Up | KeyCode.W => prevQuestion
        case KeyCode.Down | KeyCode.S => nextQuestion
        case KeyCode.Space => toggleInvalid
      } >> e.preventDefaultCB
    }

    def qaField(s: State, sentence: Vector[String], highlightedAnswers: Map[Int, Answer])(index: Int) = {
      val isFocused = s.curQuestion == index
      val answer = s.answers(index)

      <.div(
        ^.overflow := "hidden",
        <.div(
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "55px",
          (^.backgroundColor := "#E01010").when(answer.isInvalid),
          ^.onClick --> toggleInvalidAtIndex(highlightedAnswers)(index),
          "Invalid"
        ),
        <.span(
          Styles.bolded.when(isFocused),
          Styles.uncomfortableOrange.when(s.answers(s.curQuestion).getRedundant.nonEmptyAnd(_.other == index)),
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curQuestion.set(index)),
          questions(index)
        ),
        <.div(
          Styles.answerIndicator,
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.width := "25px",
          "-->".when(isFocused)
        ),
        <.div(
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          answer match {
            case InvalidQuestion => <.span(
              ^.color := "#CCCCCC",
              "N/A"
            )
            case Redundant(other) => <.span(
              ^.color := "#CCCCCC",
              "Redundant with ", <.i(questions(other)))
            case Answer(spans) if spans.isEmpty && isFocused =>
              <.span(
                ^.color := "#CCCCCC",
                "Highlight answer above, move with arrow keys or mouse")
            case Answer(spans) if isFocused => // spans nonempty
              (spans.flatMap { span =>
                 List(
                   <.span(
                     Text.renderSpan(sentence, span)
                   ),
                   <.span(" / ")
                 )
               } ++ List(<.span(^.color := "#CCCCCC", "Highlight to add an answer"))).toVdomArray
            case Answer(spans) => spans.map(Text.renderSpan(sentence, _)).mkString(" / ")
          }
        )
      )
    }

    def render(state: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri, request = QASRLValidationApiRequest(workerIdOpt, prompt.id), render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(QASRLValidationApiResponse(workerInfoOpt, sentence), _) =>
              import state._

              val agreementOpt = workerInfoOpt.map(_.agreement)
              val remainingInAgreementGracePeriodOpt = workerInfoOpt
                .map(info => QASRLSettings.validationAgreementGracePeriod - info.numAssignmentsCompleted)
                .filter(_ > 0)
              val numAssignmentsCompleted = workerInfoOpt.fold(0)(_.numAssignmentsCompleted)

              SpanHighlighting(
                SpanHighlightingProps(
                  isEnabled = !isNotAssigned && answers(curQuestion).isAnswer,
                  update = updateCurrentAnswers, render = {
                    case (hs @ SpanHighlightingState(spans, status), SpanHighlightingContext(_, hover, touch, cancelHighlight)) =>
                      val curVerbIndex = prompt.qaPairs(curQuestion).verbIndex
                      val inProgressAnswerOpt = SpanHighlightingStatus.highlighting.getOption(status).map {
                        case Highlighting(_, anchor, endpoint) => ContiguousSpan(anchor, endpoint)
                      }
                      val curAnswers = spans(curQuestion)
                      val otherAnswers = (spans - curQuestion).values.flatten
                      val highlightedAnswers = prompt.qaPairs.indices.map(i =>
                        i -> Answer(spans(i).map(_.indices))
                      ).toMap

                      val isCurrentInvalid = answers(curQuestion).isInvalid
                      val touchWord = touch(curQuestion)
                      <.div(
                        ^.classSet1("container-fluid"),
                        ^.onClick --> cancelHighlight,
                        <.div(
                          instructions,
                          ^.margin := "5px"
                        ),
                        agreementOpt.whenDefined(agreement =>
                          <.div(
                            ^.classSet1("card"),
                            ^.margin := "5px",
                            <.p(
                              """Your responses agree with others """,
                              <.span(
                                if(agreement <= QASRLSettings.validationAgreementBlockingThreshold) {
                                  Styles.badRed
                                } else if(agreement <= QASRLSettings.validationAgreementBlockingThreshold + 0.05) {
                                  TagMod(Styles.uncomfortableOrange, Styles.bolded)
                                } else {
                                  Styles.goodGreen
                                },
                                f"${agreement * 100.0}%.1f%%"
                              ),
                              f""" of the time. This must remain above ${QASRLSettings.validationAgreementBlockingThreshold * 100.0}%.1f%%""",
                              remainingInAgreementGracePeriodOpt.fold(".")(remaining =>
                                s" after the end of a grace period ($remaining verbs remaining)."
                              )
                            )
                          )
                        ),
                        <.div(
                          ^.classSet1("card"),
                          ^.margin := "5px",
                          ^.padding := "5px",
                          ^.tabIndex := 0,
                          ^.onFocus --> scope.modState(State.isInterfaceFocused.set(true)),
                          ^.onBlur --> scope.modState(State.isInterfaceFocused.set(false)),
                          ^.onKeyDown ==> ((e: ReactKeyboardEvent) => handleKey(highlightedAnswers)(e) >> cancelHighlight),
                          ^.position := "relative",
                          <.div(
                            ^.position := "absolute",
                            ^.top := "20px",
                            ^.left := "0px",
                            ^.width := "100%",
                            ^.height := "100%",
                            ^.textAlign := "center",
                            ^.color := "rgba(48, 140, 20, .3)",
                            ^.fontSize := "48pt",
                            (if(isNotAssigned) "Accept assignment to start" else "Click here to start")
                          ).when(!isInterfaceFocused),
                          MultiContigSpanHighlightableSentence(
                            MultiContigSpanHighlightableSentenceProps(
                              sentence = sentence,
                              styleForIndex = i => TagMod(Styles.specialWord, Styles.niceBlue).when(i == curVerbIndex),
                              highlightedSpans = (
                                inProgressAnswerOpt.map(_ -> (^.backgroundColor := "#FF8000")) ::
                                (curAnswers.map(_ -> (^.backgroundColor := "#FFFF00")) ++
                                otherAnswers.map(_ -> (^.backgroundColor := "#DDDDDD"))).map(Some(_))).flatten,
                              hover = hover(state.curQuestion),
                              touch = touch(state.curQuestion),
                              render = (elements =>
                                <.p(
                                  Styles.largeText,
                                  Styles.unselectable,
                                  elements.toVdomArray)
                              ))
                          ),
                          <.ul(
                            ^.classSet1("list-unstyled"),
                            (0 until questions.size)
                              .map(qaField(state, sentence, highlightedAnswers))
                              .map(field => <.li(^.display := "block", field))
                              .toVdomArray
                          ),
                          <.p(s"Bonus: ${dollarsToCents(validationBonus(questions.size))}c")
                        ),
                        <.div(
                          ^.classSet1("form-group"),
                          ^.margin := "5px",
                          <.textarea(
                            ^.classSet1("form-control"),
                            ^.name := FieldLabels.feedbackLabel,
                            ^.rows := 3,
                            ^.placeholder := "Feedback? (Optional)"
                          )
                        ),
                        <.input(
                          ^.classSet1("btn btn-primary btn-lg btn-block"),
                          ^.margin := "5px",
                          ^.`type` := "submit",
                          ^.disabled := !state.answers.forall(_.isComplete),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := (
                            if(isNotAssigned) "You must accept the HIT to submit results"
                            else if(!state.answers.forall(_.isComplete)) "You must respond to all questions to submit results"
                            else "Submit"
                          ))
                      )
                  }
                )
              )
          }
        )
      )
    }
  }

  val FullUI = ScalaComponent.builder[Unit]("Full UI")
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(_.backend.updateResponse)
    .build

}
