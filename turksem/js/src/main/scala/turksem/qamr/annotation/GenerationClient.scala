package turksem.qamr.annotation

import cats.implicits._

import turksem._
import turksem.qamr._
import turksem.util._

import turkey.tasks._

import nlpdata.util.Text

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

class GenerationClient[SID : Reader : Writer](
  instructions: ReactTag,
  requireWhAtQuestionBeginning: Boolean,
  settings: PipelineSettings)(
  implicit promptReader: Reader[GenerationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[WordedQAPair]] // same as above
) extends TaskClient[GenerationPrompt[SID], List[WordedQAPair]] {

  import settings._

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[GenerationApiRequest[SID], GenerationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int)] // keyword INDEX among keywords, qa index, answer word index
  import HighlightingComponent._

  @Lenses case class State(
    qaGroups: List[List[WordedQAPair]], // each group == 1 keyword
    curFocus: (Int, Int)) {
  }
  object State {
    val initial = State(prompt.keywords.map(kw => List(emptyQA(kw))), (0, 0))
  }

  def emptyQA(keyword: Int) = WordedQAPair(keyword, "", Set.empty[Int])

  def isComplete(wqa: WordedQAPair) = !wqa.question.isEmpty && !wqa.answer.isEmpty &&
    (!requireWhAtQuestionBeginning || beginsWithWh(wqa.question))

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def addQAFields: (State => State) =
      State.qaGroups.modify(groups =>
        groups.map(group =>
          if(group.filter(isComplete).size == group.size) group ++ List(emptyQA(group.head.wordIndex))
          else group
        )
      )

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.modify(groups =>
          groups.zipWithIndex.map {
            case (group, gIndex) =>
              group.zipWithIndex.map {
                case (WordedQAPair(keyword, question, _), qaIndex) => WordedQAPair(
                  keyword, question, hs.span.collect {
                    case (`gIndex`, `qaIndex`, aIndex) => aIndex
                  }.toSet)
              }
          }
        ) andThen addQAFields
      )

    def updateResponse: Callback = scope.state.map { st =>
      setResponse(st.qaGroups.flatten.filter(isComplete))
    }

    def qaField(s: State, sentence: Vector[String], groupIndex: Int, qaIndex: Int) = s match {
      case State(qaGroups, curFocus) =>
        val isFocused = curFocus == (groupIndex, qaIndex)
        val numQAsInGroup = qaGroups(groupIndex).size
        val WordedQAPair(_, question, answer) = s.qaGroups(groupIndex)(qaIndex)
        val charsLeft = questionCharLimit - question.length
        val isAnswerEmpty = answer.isEmpty
        val nextBonus = bonusFor(qaGroups.map(_.drop(1)).flatten.filter(isComplete).size + 1)
        <.div(
          ^.overflow := "hidden",
          <.div(
            Styles.badRed,
            ^.float := "left",
            ^.width := "10px",
            ^.minHeight := "1px",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.textAlign := "right",
            (charsLeft <= 10 && isFocused) ?= s"$charsLeft"
          ),
          <.input(
            isNotAssigned ?= (^.disabled := true),
            (!question.isEmpty && !isFocused && requireWhAtQuestionBeginning && !beginsWithWh(question)) ?= (^.backgroundColor := "#FF8888"),
            ^.float := "left",
            ^.`type` := "text",
            ^.placeholder := (
              if(qaIndex == 0) "Question (required)"
              else s"Question (+${math.round(100 * nextBonus).toInt}c)"
            ),
            ^.margin := "1px",
            ^.padding := "1px",
            // ^.width := "240px",
            ^.size := questionCharLimit,
            ^.maxLength := questionCharLimit,
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  State.qaGroups.modify(groups =>
                    groups.updated(
                      groupIndex,
                      groups(groupIndex).updated(
                        qaIndex,
                        groups(groupIndex)(qaIndex).copy(question = newValue))
                    )) andThen addQAFields)
              }),
            ^.onFocus --> scope.modState(State.curFocus.set((groupIndex, qaIndex))),
            ^.value := question
          ),
          <.div(
            Styles.answerIndicator,
            ^.float := "left",
            ^.minHeight := "1px",
            ^.width := "25px",
            isFocused ?= "-->"
          ),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            isAnswerEmpty ?= (^.color := "#CCCCCC"),
            if(isAnswerEmpty && isFocused) {
              "Highlight your answer above"
            } else {
              Text.renderSpan(sentence, answer)
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = GenerationApiRequest(prompt.id),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(GenerationApiResponse(sentence), _) =>
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateHighlights, render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(setSpan, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val curCompleteQAPairs = s.qaGroups.flatten.filter(isComplete)

                      val curPotentialBonus = (1 to (curCompleteQAPairs.size - s.qaGroups.size))
                        .map(bonusFor).sum

                      val (curGroupIndex, curQAIndex) = s.curFocus

                      val curAnswer = spans.collect {
                        case (`curGroupIndex`, `curQAIndex`, ansIndex) => ansIndex
                      }
                      def touchWord(i: Int) = touchElement((curGroupIndex, curQAIndex, i))
                      <.div(
                        ^.onMouseUp --> stopHighlight,
                        ^.onMouseDown --> startHighlight,
                        Styles.mainContent,
                        <.p(<.span(Styles.badRed, """ Please read the detailed instructions at the bottom before you begin, """),
                            """ so you can maximize your bonuses and avoid losing your qualification. """,
                            """ To begin working on this HIT, please request the question-answer writing accuracy qualification.
                                It is auto-granted. Also, while there may be few HITs available at any one time,
                                more will be continuously uploaded as they are completed. """),
                        <.hr(),
                        <.p(
                          Styles.unselectable,
                          Text.render(
                            sentence.indices.toList,
                            (index: Int) => sentence(index),
                            (nextIndex: Int) => List(
                              <.span(
                                ^.backgroundColor := (
                                  if(curAnswer.contains(nextIndex) && curAnswer.contains(nextIndex - 1)) {
                                    "#FFFF00"
                                  } else {
                                    "transparent"
                                  }),
                                " ")),
                            (index: Int) => List(
                              <.span(
                                prompt.keywords.contains(index) ?= Styles.specialWord,
                                prompt.keywords.contains(index) ?= Styles.niceBlue,
                                ^.backgroundColor := (
                                  if(curAnswer.contains(index)) {
                                    "#FFFF00"
                                  } else {
                                    "transparent"
                                  }
                                ),
                                ^.onMouseMove --> touchWord(index),
                                ^.onMouseDown ==> (
                                  (e: ReactEventI) => if(curAnswer.contains(index)) {
                                    e.stopPropagation
                                    startErase >> touchWord(index)
                                  } else {
                                    startHighlight >> touchWord(index)
                                  }
                                ),
                                Text.normalizeToken(sentence(index))
                              ))
                          )),
                        <.div(
                          (0 until s.qaGroups.size).map(groupIndex =>
                            <.div(
                              <.p(
                                Styles.bolded,
                                Text.normalizeToken(sentence(prompt.keywords(groupIndex)))
                              ),
                              <.ul(
                                Styles.listlessList,
                                (0 until s.qaGroups(groupIndex).size).map(qaIndex =>
                                  <.li(
                                    ^.display := "block",
                                    qaField(s, sentence, groupIndex, qaIndex)
                                  )
                                )
                              )
                            )
                          )
                        ),
                        <.p(
                          "Potential bonus so far: ",
                          <.span(
                            curPotentialBonus > 0 ?= Styles.goodGreen,
                            curPotentialBonus > 0 ?= Styles.bolded,
                            s"${math.round(100 * curPotentialBonus).toInt}c"
                          )
                        ),
                        <.p(
                          <.input(
                            ^.`type` := "text",
                            ^.name := feedbackLabel,
                            ^.placeholder := "Feedback? (Optional)",
                            ^.margin := "1px",
                            ^.padding := "1px",
                            ^.width := "484px"
                          )
                        ),
                        <.input(
                          ^.`type` := "submit",
                          ^.disabled := !s.qaGroups.forall(_.filter(isComplete).size > 0),
                          ^.id := submitButtonLabel,
                          ^.value := "submit"),
                        instructions
                      )
                  }))
          }))
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

}
