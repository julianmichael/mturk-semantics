package turksem.iqa

import turkey.tasks._
import cats.implicits._

import turksem._
import turksem.qasrl._
import turksem.iqa._
import turksem.iqa.AdaptiveQuestionGuesser.ops._
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

class IQAClient[SID : Writer : Reader](
  instructions: VdomTag)(
  // implicit settings: IQASettings,
  implicit promptReader: Reader[IQAPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[IQAResponse], // same as above
  ajaxRequestWriter: Writer[IQAAjaxRequest[SID]] // "
) extends TaskClient[IQAPrompt[SID], IQAResponse, IQAAjaxRequest[SID]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val AsyncContentComponent = new AsyncContentComponent[IQAAjaxResponse]
  import AsyncContentComponent._
  val SpanHighlightingComponent = new SpanHighlightingComponent[(Int, Int)] // trigger group, question
  import SpanHighlightingComponent._

  import MultiContigSpanHighlightableSentenceComponent._

  @Lenses case class State(
    questioning: QuestioningState,
    questionGuesser: CountBasedQuestionGuesser,
    curFocus: (Int, Int),
    isInterfaceFocused: Boolean) {
    def allQAs = questioning.triggerGroups.flatMap(_.qas)
  }
  object State {
    def initial = State(
      QuestioningState.empty, AdaptiveQuestionGuesser[CountBasedQuestionGuesser].empty, (0, 0), false
    )
    def initFromResponse(response: IQAAjaxResponse) = {
      val qState = QuestioningState.initFromSentence(response.inflectedTokens)
      val firstTriggerWord = qState.triggerGroups.head.trigger
      val firstQuestion = response.questionGuesser.guessForTrigger(qState, firstTriggerWord).get
      val firstQA = TemplatedQA(firstQuestion, Answer(Nil, ""))
      val populatedQState = QuestioningState.triggerGroups
        .composeOptional(Optics.index(0))
        .composeLens(TriggerGroup.qas)
        .set(Vector(firstQA))(qState)
      State(populatedQState, response.questionGuesser, (0, 0), false)
    }
  }

  val triggerGroupsL = State.questioning.composeLens(QuestioningState.triggerGroups)

  val eachTriggerGroupT = triggerGroupsL.composeTraversal(Optics.each)

  def triggerGroupO(groupIndex: Int) =
    triggerGroupsL.composeOptional(Optics.index(groupIndex))

  def triggerGroupL(groupIndex: Int) = Lens[State, TriggerGroup](
    s => triggerGroupO(groupIndex).getOption(s).get)(
    tqa => s => triggerGroupO(groupIndex).set(tqa)(s))

  def qasL(groupIndex: Int) =
    triggerGroupL(groupIndex).composeLens(TriggerGroup.qas)

  def qaO(groupIndex: Int, questionIndex: Int) =
    qasL(groupIndex).composeOptional(Optics.index(questionIndex))

  def qaL(groupIndex: Int, questionIndex: Int) = Lens[State, TemplatedQA](
    s => qaO(groupIndex, questionIndex).getOption(s).get)(
    tqa => s => qaO(groupIndex, questionIndex).set(tqa)(s))

  def questionL(groupIndex: Int, questionIndex: Int) =
    qaL(groupIndex, questionIndex).composeLens(TemplatedQA.question)

  def judgmentL(groupIndex: Int, questionIndex: Int) =
    qaL(groupIndex: Int, questionIndex).composeLens(TemplatedQA.judgment)

  def answerO(groupIndex: Int, questionIndex: Int) =
    judgmentL(groupIndex: Int, questionIndex).composePrism(QuestionJudgment.answer)

  def answerSpansO(groupIndex: Int, questionIndex: Int) =
    answerO(groupIndex: Int, questionIndex).composeLens(Answer.spans)

  def answerStandinO(groupIndex: Int, questionIndex: Int) =
    answerO(groupIndex: Int, questionIndex).composeLens(Answer.standin)

  val curGroupL = Lens[State, TriggerGroup](
    s => s.questioning.triggerGroups(s.curFocus._1))(
    group => s => triggerGroupL(s.curFocus._1).set(group)(s))

  val curQAsL = curGroupL.composeLens(TriggerGroup.qas)

  val curQAL = Lens[State, TemplatedQA](
    s => qaL(s.curFocus._1, s.curFocus._2).get(s))(
    tqa => s => qaL(s.curFocus._1, s.curFocus._2).set(tqa)(s))

  val curQuestionL = curQAL.composeLens(TemplatedQA.question)
  val curJudgmentL = curQAL.composeLens(TemplatedQA.judgment)
  val curAnswerO = curJudgmentL.composePrism(QuestionJudgment.answer)
  val curAnswerSpansO = curAnswerO.composeLens(Answer.spans)
  val curAnswerStandinO = curAnswerO.composeLens(Answer.standin)

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def isQAFinished(qa: TemplatedQA): Boolean =
      qa.judgment.getAnswer.emptyOr(_.spans.nonEmpty)

    def updateResponse: Callback = scope.state.map { state =>
      setResponse(
        IQAResponse(
          state.questioning.triggerGroups.flatMap {
            case TriggerGroup(trigger, _, qas) =>
              qas.filter(isQAFinished).map(trigger -> _)
          }.toList
        )
      )
    }

    def generateNewQuestionsIfNecessary(state: State): Callback = {
      // prevent infinite loop from successive updates
      val needAnyNewQuestion = state.questioning.triggerGroups.exists(_.qas.forall(isQAFinished))
      if(!needAnyNewQuestion) Callback.empty
      else {
        val changeState = eachTriggerGroupT.modify { curGroup =>
          if(curGroup.qas.forall(isQAFinished)) {
            state.questionGuesser
              .guessForTrigger(state.questioning, curGroup.trigger)
              .headOption.fold(curGroup) { newQ =>
              val newQA = TemplatedQA(newQ, Answer(Nil, ""))
              curGroup.copy(qas = curGroup.qas :+ newQA)
            }
          } else curGroup
        }
        if(changeState(state) == state) Callback.empty
        else scope.modState(changeState)
      }
    }

    def update(state: State): Callback =
      generateNewQuestionsIfNecessary(state) >> updateResponse

    def readyToSubmit(state: State): Boolean = {
      true
      // state.questionGuesser.guess(state.questioning).isEmpty ||
      //   state.questioning.qas.filter(isQAFinished).size >= 5
    }

    def getNewAnswerSpan(sentence: Vector[InflectionalWord], spans: List[ContiguousSpan]) = {
      val standin = spans.headOption.fold("")(span => Text.renderSpan(sentence.map(_.token), span.indices))
      Answer(spans, standin)
    }

    def updateCurrentAnswers(sentence: Vector[InflectionalWord])(highlightingState: SpanHighlightingState) =
      scope.state >>= (st =>
        scope.modState(answerO(st.curFocus._1, st.curFocus._2).set(getNewAnswerSpan(sentence, highlightingState.spans(st.curFocus))))
      )

    def cycleValidityAtIndex(
      sentence: Vector[InflectionalWord],
      highlightedSpans: Map[(Int, Int), List[ContiguousSpan]])(
      index: (Int, Int)
    ) =
      scope.modState(
        judgmentL(index._1, index._2).modify {
          case BadQuestion => NoAnswer
          case NoAnswer => getNewAnswerSpan(sentence, highlightedSpans(index))
          case Answer(_, _) => BadQuestion
        }
      )

    def toggleNoAnswerAtIndex(
      sentence: Vector[InflectionalWord],
      highlightedSpans: Map[(Int, Int), List[ContiguousSpan]])(
      index: (Int, Int)) =
      scope.modState(
        judgmentL(index._1, index._2).modify {
          case NoAnswer => getNewAnswerSpan(sentence, highlightedSpans(index))
          case _ => NoAnswer
        }
      )

    def toggleBadQuestionAtIndex(
      sentence: Vector[InflectionalWord],
      highlightedSpans: Map[(Int, Int), List[ContiguousSpan]])(
      index: (Int, Int)) =
      scope.modState(
        judgmentL(index._1, index._2).modify {
          case BadQuestion => getNewAnswerSpan(sentence, highlightedSpans(index))
          case _ => BadQuestion
        }
      )

    def handleKey(
      sentence: Vector[InflectionalWord],
      highlightedAnswers: Map[(Int, Int), List[ContiguousSpan]])(
      e: ReactKeyboardEvent): Callback = {
      // def nextQuestion = scope.modState(s =>
      //   State.curFocus.modify(i => (i + 1) % s.questioning.qas.size)(s)
      // )
      // def prevQuestion = scope.modState(s =>
      //   State.curFocus.modify(i => (i + s.questioning.qas.size - 1) % s.questioning.qas.size)(s)
      // )
      // def cycleValidity = scope.zoomStateL(State.curFocus).state >>= cycleValidityAtIndex(sentence, highlightedAnswers)

      // TODO do we really want key controls? preventing them for now (but yes we do, soon)
      if(true || isNotAssigned) {
        Callback.empty
      } else CallbackOption.keyCodeSwitch(e) {
        case _ => Callback.empty
        // case KeyCode.Up => prevQuestion
        // case KeyCode.Down => nextQuestion
        // case KeyCode.Space => cycleValidity
      } >> e.preventDefaultCB
    }

    def qaField(s: State, sentence: Vector[InflectionalWord], highlightedAnswers: Map[(Int, Int), List[ContiguousSpan]])(index: (Int, Int)) = {
      <.div()
      val isFocused = s.curFocus == index
      val judgment = judgmentL(index._1, index._2).get(s)
      // val answer = s.answers(index)

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
          (^.backgroundColor := "#E01010").when(judgment.isBadQuestion),
          ^.onClick --> toggleBadQuestionAtIndex(sentence, highlightedAnswers)(index),
          "Invalid"
        ),
        <.div(
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "95px",
          (^.backgroundColor := "#FFA500").when(judgment.isNoAnswer),
          ^.onClick --> toggleNoAnswerAtIndex(sentence, highlightedAnswers)(index),
          "No Answer"
        ),
        <.span(// TODO fancy stuff with changing question tense, aspect, etc. ?
          Styles.bolded.when(isFocused),
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curFocus.set(index)),
          s.questioning.renderQuestion(index._1, index._2).get
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
          judgment match {
            case BadQuestion | NoAnswer => <.span(
              ^.color := "#CCCCCC",
              "N/A"
            )
            case Answer(spans, _) if spans.isEmpty && isFocused =>
              <.span(
                ^.color := "#CCCCCC",
                "Highlight answer above, change questions with the mouse")
            case Answer(spans, standin) if isFocused => // spans nonempty
              <.input(
                ^.onChange ==> (
                  (e: ReactEventFromInput) => {
                    val newStandin = e.target.value
                    scope.modState(answerStandinO(index._1, index._2).set(newStandin))
                  }),
                ^.value := standin)
            case Answer(spans, standin) =>
              <.span(standin)
          }
        )
      )
    }

    def render(state: State) = {
      AsyncContent(
        AsyncContentProps(
          getContent = () => makeAjaxRequest(IQAAjaxRequest(prompt.id)),
          willLoad = r => scope.setState(State.initFromResponse(r)),
          render = {
            case Loading => <.div("Retrieving data...")
            case Loaded(IQAAjaxResponse(sentence, _)) =>
              SpanHighlighting(
                SpanHighlightingProps(
                  isEnabled = !isNotAssigned && curJudgmentL.get(state).isAnswer,
                  enableSpanOverlap = true,
                  update = updateCurrentAnswers(sentence), render = {
                    case (hs @ SpanHighlightingState(spans, status), SpanHighlightingContext(_, hover, touch, cancelHighlight)) =>
                      val curTriggerIndices = curQuestionL.get(state).arguments.collect {
                        case SentenceWord(i) => i
                      }.toSet
                      val inProgressAnswerOpt = SpanHighlightingStatus.highlighting.getOption(status).map {
                        case Highlighting(_, anchor, endpoint) => ContiguousSpan(anchor, endpoint)
                      }
                      val curAnswerSpans = curAnswerSpansO.getOption(state).getOrElse(Nil)
                      val touchWord = touch(state.curFocus)

                      // val isCurrentInvalid = answers(curFocus).isInvalid
                      <.div(
                        ^.classSet1("container-fluid"),
                        ^.onClick --> cancelHighlight,
                        <.div(
                          instructions,
                          ^.margin := "5px"
                        ),
                        <.div(
                          ^.classSet1("card"),
                          ^.margin := "5px",
                          ^.padding := "5px",
                          // ^.tabIndex := 0,
                          // ^.onFocus --> scope.modState(State.isInterfaceFocused.set(true)),
                          // ^.onBlur --> scope.modState(State.isInterfaceFocused.set(false)),
                          ^.onKeyDown ==> ((e: ReactKeyboardEvent) => handleKey(sentence, spans)(e) >> cancelHighlight),
                          ^.position := "relative",
                          // <.div(
                          //   ^.position := "absolute",
                          //   ^.top := "20px",
                          //   ^.left := "0px",
                          //   ^.width := "100%",
                          //   ^.height := "100%",
                          //   ^.textAlign := "center",
                          //   ^.color := "rgba(48, 140, 20, .3)",
                          //   ^.fontSize := "48pt",
                          //   (if(isNotAssigned) "Accept assignment to start" else "Click here to start")
                          // ).when(!state.isInterfaceFocused),
                          MultiContigSpanHighlightableSentence(
                            MultiContigSpanHighlightableSentenceProps(
                              sentence = sentence.map(_.token),
                              styleForIndex = i => TagMod(Styles.niceBlue).when(curTriggerIndices.contains(i)),
                              highlightedSpans = (
                                inProgressAnswerOpt.map(_ -> (^.backgroundColor := "#FF8000")) ::
                                  curAnswerSpans.map(x => Some(x -> (^.backgroundColor := "#FFFF00")))
                              ).flatten,
                              hover = hover(state.curFocus),
                              touch = touch(state.curFocus),
                              render = (elements =>
                                <.p(
                                  Styles.largeText,
                                  Styles.unselectable,
                                  elements.toVdomArray)
                              ))
                          ),
                          <.div(
                            ^.classSet1("list-unstyled"),
                            (0 until triggerGroupsL.get(state).size).toVdomArray { groupIndex =>
                              val trigger = triggerGroupL(groupIndex).get(state).trigger
                              <.div(
                                ^.key := s"group-$groupIndex",
                                <.h3(trigger.token),
                                <.ul(
                                  (0 until qasL(groupIndex).get(state).size).toVdomArray { questionIndex =>
                                    <.li(
                                      ^.key := s"question-$questionIndex",
                                      ^.display := "block",
                                      qaField(state, sentence, spans)(groupIndex -> questionIndex))
                                })
                                )
                            }
                          )
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
                          ^.disabled := readyToSubmit(state),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := (
                            if(isNotAssigned) "You must accept the HIT to submit results"
                            else if(!readyToSubmit(state)) "You must respond to at least 5 questions to submit results"
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
    .componentDidUpdate(context => context.backend.update(context.currentState))
    .build

}
