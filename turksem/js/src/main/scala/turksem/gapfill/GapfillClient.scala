package turksem.gapfill

import turkey.tasks._

import cats.implicits._
import cats.Traverse

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

import QuestionGuesser.ops._

class GapfillClient[
  SID : Writer : Reader,
  ClientGuesser : Writer : Reader : QuestionGuesser](
  instructions: VdomTag)(
  implicit promptReader: Reader[GapfillPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[GapfillResponse], // same as above
  ajaxRequestWriter: Writer[GapfillAjaxRequest[SID, ClientGuesser]], // "
  ajaxResponseReader: ResponseReader[GapfillAjaxRequest[SID, ClientGuesser]] // "
) extends TaskClient[GapfillPrompt[SID], GapfillResponse, GapfillAjaxRequest[SID, ClientGuesser]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val AsyncContentComponent = new AsyncContentComponent[GapfillAjaxResponse[ClientGuesser]]
  import AsyncContentComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int)] // trigger group, question, answer word index
  import HighlightingComponent._

  import HighlightableSentenceComponent._

  @Lenses case class State(
    questioning: QuestioningState,
    questionGuesser: ClientGuesser,
    curFocus: (Int, Int),
    isInterfaceFocused: Boolean) {
    def allQAs = questioning.triggerGroups.flatMap(_.qas)
  }
  object State {
    def initial: State = null
    def initFromResponse(response: GapfillAjaxResponse[ClientGuesser]) = {
      val qState = QuestioningState.initFromSentence(response.inflectedTokens)
      // val firstTriggerWord = qState.triggerGroups.head.trigger
      val firstQuestion = response.questionGuesser.guessForTrigger(qState, 0).get
      val firstQA = JudgedQuestion(firstQuestion, Answer(Set.empty[Int]))
      val populatedQState = QuestioningState.triggerGroups
        .composeOptional(Optics.index(0))
        .composeLens(TriggerGroup.qas)
        .set(Vector(firstQA))(qState)
      State(populatedQState, response.questionGuesser, (0, 0), false)
    }
  }

  def indexingIso[F[_]: Traverse, A] = Iso[F[A], F[(A, Int)]](_.zipWithIndex)(_.map(_._1))
  def eachIndexed[F[_]: Traverse, A](
    implicit eachInstance: monocle.function.Each[F[(A, Int)], (A, Int)]
  ) = indexingIso[F, A].composeTraversal(Optics.each)

  val triggerGroupsL = State.questioning.composeLens(QuestioningState.triggerGroups)

  val eachTriggerGroupT = triggerGroupsL.composeTraversal(Optics.each)
  val eachTriggerGroupIndexedT = triggerGroupsL.composeTraversal(eachIndexed)

  def triggerGroupO(groupIndex: Int) =
    triggerGroupsL.composeOptional(Optics.index(groupIndex))

  def triggerGroupL(groupIndex: Int) = Lens[State, TriggerGroup](
    s => triggerGroupO(groupIndex).getOption(s).get)(
    tqa => s => triggerGroupO(groupIndex).set(tqa)(s))

  def qasL(groupIndex: Int) =
    triggerGroupL(groupIndex).composeLens(TriggerGroup.qas)

  def qaO(groupIndex: Int, questionIndex: Int) =
    qasL(groupIndex).composeOptional(Optics.index(questionIndex))

  def qaL(groupIndex: Int, questionIndex: Int) = Lens[State, JudgedQuestion](
    s => qaO(groupIndex, questionIndex).getOption(s).get)(
    tqa => s => qaO(groupIndex, questionIndex).set(tqa)(s))

  def questionL(groupIndex: Int, questionIndex: Int) =
    qaL(groupIndex, questionIndex).composeLens(JudgedQuestion.question)

  def judgmentL(groupIndex: Int, questionIndex: Int) =
    qaL(groupIndex: Int, questionIndex).composeLens(JudgedQuestion.judgment)

  def answerO(groupIndex: Int, questionIndex: Int) =
    judgmentL(groupIndex: Int, questionIndex).composePrism(QuestionJudgment.answer)

  val curTriggerFocus = State.curFocus.composeLens(Optics.first)
  val curQuestionFocus = State.curFocus.composeLens(Optics.second)

  // def answerSpanO(groupIndex: Int, questionIndex: Int) =
  //   answerO(groupIndex: Int, questionIndex).composeLens(Answer.span)

  // def answerStandinO(groupIndex: Int, questionIndex: Int) =
  //   answerO(groupIndex: Int, questionIndex).composeLens(Answer.standin)

  val curGroupL = Lens[State, TriggerGroup](
    s => s.questioning.triggerGroups(s.curFocus._1))(
    group => s => triggerGroupL(s.curFocus._1).set(group)(s))

  val curQAsL = curGroupL.composeLens(TriggerGroup.qas)

  val curQAL = Lens[State, JudgedQuestion](
    s => qaL(s.curFocus._1, s.curFocus._2).get(s))(
    tqa => s => qaL(s.curFocus._1, s.curFocus._2).set(tqa)(s))

  val curQuestionL = curQAL.composeLens(JudgedQuestion.question)
  val curJudgmentL = curQAL.composeLens(JudgedQuestion.judgment)
  val curAnswerO = curJudgmentL.composePrism(QuestionJudgment.answer)
  val curAnswerSpanO = curAnswerO.composeLens(Answer.span)
  // val curAnswerStandinO = curAnswerO.composeLens(Answer.standin)

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def isQAFinished(qa: JudgedQuestion): Boolean =
      qa.judgment.getAnswer.emptyOr(_.span.nonEmpty)

    def updateResponse: Callback = scope.state.map { state =>
      setResponse(
        GapfillResponse(
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
        val changeState = eachTriggerGroupIndexedT.modify { case (curGroup, curGroupIndex) =>
          if(curGroup.qas.forall(isQAFinished)) {
            state.questionGuesser
              .guessForTrigger(state.questioning, curGroupIndex)
              .headOption.fold(curGroup) { newQ =>
              val newQA = JudgedQuestion(newQ, Answer(Set.empty[Int]))
              curGroup.copy(qas = curGroup.qas :+ newQA)
            } -> curGroupIndex
          } else curGroup -> curGroupIndex
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

    def getNewAnswerSpan(sentence: Vector[InflectionalWord], span: Set[Int]) = {
      // val standin = spans.headOption.fold("")(span => Text.renderSpan(sentence.map(_.token), span.indices))
      Answer(span)
    }

    def updateCurrentAnswers(sentence: Vector[InflectionalWord])(highlightingState: HighlightingState) = {
      val highlightedSpans: Map[(Int, Int), Set[Int]] = highlightingState.span
        .groupBy(t => (t._1, t._2))
        .map { case (index, tuples) => index -> tuples.map(_._3) }
      scope.state >>= (st =>
        scope.modState(answerO(st.curFocus._1, st.curFocus._2).set(getNewAnswerSpan(sentence, highlightedSpans(st.curFocus))))
      )
    }

    def toggleBadQuestionAtIndex(
      sentence: Vector[InflectionalWord],
      highlightedSpans: Map[(Int, Int), Set[Int]])(
      index: (Int, Int)) =
      scope.modState(
        judgmentL(index._1, index._2).modify {
          case BadQuestion => getNewAnswerSpan(sentence, highlightedSpans(index))
          case _ => BadQuestion
        }
      )

    // def toggleCurrentBadQuestion

    sealed trait CycleDirection
    case object Forward extends CycleDirection
    case object Backward extends CycleDirection

    def cycle(
      index: Int,
      direction: CycleDirection,
      boundInclusive: Int
    ): Either[CycleDirection, Int] = direction match {
      case Forward => if(index == boundInclusive) Left(Forward) else Right(index + 1)
      case Backward => if(index == 0) Left(Backward) else Right(index - 1)
    }

    def handleKey(
      sentence: Vector[InflectionalWord],
      highlightedSpans: Map[(Int, Int), Set[Int]])(
      e: ReactKeyboardEvent): Callback = {
      def moveQuestion(direction: CycleDirection) = scope.modState(s =>
        State.curFocus.modify {
          case (groupIndex, qaIndex) =>
            cycle(qaIndex, direction, s.questioning.triggerGroups(groupIndex).qas.size) match {
              case Right(newQAIndex) => (groupIndex, newQAIndex)
              case Left(direc) => cycle(groupIndex, direc, s.questioning.triggerGroups.size) match {
                case Right(newGroupIndex) =>
                  val newQAIndex = direc match {
                    case Forward => 0
                    case Backward => s.questioning.triggerGroups(newGroupIndex).qas.size - 1
                  }
                  (newGroupIndex, newQAIndex)
                case Left(Forward) => (0, 0)
                case Left(Backward) =>
                  val lastTriggerGroupIndex = s.questioning.triggerGroups.size - 1
                  (lastTriggerGroupIndex, s.questioning.triggerGroups(lastTriggerGroupIndex).qas.size - 1)
              }
            }
        }(s)
      )
      // def cycleValidity = scope.zoomStateL(State.curFocus).state >>= cycleValidityAtIndex(sentence, highlightedSpans)

      if(isNotAssigned) {
        Callback.empty
      } else CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Up => moveQuestion(Backward)
        case KeyCode.Down => moveQuestion(Forward)
        // case KeyCode.Space => cycleValidity // TODO
      } >> e.preventDefaultCB
    }

    def qaField(s: State, sentence: Vector[InflectionalWord], highlightedSpans: Map[(Int, Int), Set[Int]])(index: (Int, Int)) = {
      <.div()
      val isFocused = s.curFocus == index
      val judgedQuestion = qaL(index._1, index._2).get(s)
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
          (^.backgroundColor := "#E01010").when(judgedQuestion.judgment.isBadQuestion),
          ^.onClick --> toggleBadQuestionAtIndex(sentence, highlightedSpans)(index),
          "Invalid"
        ),
        <.span(// TODO fancy stuff with changing question tense, aspect, etc. ?
          Styles.bolded.when(isFocused),
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curFocus.set(index)),
          s.questioning.renderQuestion(judgedQuestion.question)
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
          judgedQuestion.judgment match {
            case BadQuestion => <.span(
              ^.color := "#CCCCCC",
              "N/A"
            )
            case Answer(span) if span.isEmpty && isFocused =>
              <.span(
                ^.color := "#CCCCCC",
                "Highlight answer above, change questions with the mouse")
            case Answer(span) =>
              <.span(Text.renderSpan(sentence.map(_.token), span))
          }
        )
      )
    }

    def render(state: State) = {
      AsyncContent(
        AsyncContentProps(
          getContent = () => makeAjaxRequest(GapfillAjaxRequest(prompt.id)),
          willLoad = r => scope.setState(State.initFromResponse(r)),
          render = {
            case Loading => <.div("Retrieving data...")
            case Loaded(GapfillAjaxResponse(sentence, _)) =>
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned && curJudgmentL.get(state).isAnswer,
                  update = updateCurrentAnswers(sentence), render = {
                    case (hs @ HighlightingState(span, status), HighlightingContext(_, startHighlight, startErase, stop, touchElement)) =>
                      val highlightedSpans = span.groupBy(t => (t._1, t._2)).map { case (t, ts) => t -> ts.map(_._3) }
                      val curTriggerIndices = curQuestionL.get(state).arguments.collect {
                        case SentenceWord(i) => i
                      }.toSet
                      val curAnswerSpanOpt = curAnswerSpanO.getOption(state)
                      def touchWord(wordIndex: Int) = touchElement((state.curFocus._1, state.curFocus._2, wordIndex))

                      // val isCurrentInvalid = answers(curFocus).isInvalid
                      <.div(
                        ^.classSet1("container-fluid"),
                        <.div(
                          instructions,
                          ^.margin := "5px"
                        ),
                        <.div(
                          ^.classSet1("card"),
                          ^.margin := "5px",
                          ^.padding := "5px",
                          ^.tabIndex := 0,
                          ^.onFocus --> scope.modState(State.isInterfaceFocused.set(true)),
                          ^.onBlur --> scope.modState(State.isInterfaceFocused.set(false)),
                          ^.onKeyDown ==> ((e: ReactKeyboardEvent) => handleKey(sentence, highlightedSpans)(e)),
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
                          ).when(!state.isInterfaceFocused),
                          HighlightableSentence(
                            HighlightableSentenceProps(
                              sentence = sentence.map(_.token),
                              styleForIndex = i => TagMod(Styles.niceBlue).when(curTriggerIndices.contains(i)),
                              highlightStyle = (^.backgroundColor := "#FFFF00"),
                              highlightedIndices = curAnswerSpanOpt.foldK,
                              startHighlight = startHighlight,
                              startErase = startErase,
                              touchWord = touchWord,
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
                                      qaField(state, sentence, highlightedSpans)(groupIndex -> questionIndex))
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
                          ^.disabled := false, // readyToSubmit(state),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := (
                            "Submit"
                            // if(isNotAssigned) "You must accept the HIT to submit results"
                            // else if(!readyToSubmit(state)) "You must respond to at least 5 questions to submit results"
                            // else "Submit"
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
