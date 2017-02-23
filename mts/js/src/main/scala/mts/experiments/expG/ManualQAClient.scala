package mts.experiments.expG

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._
import mts.util._
import mts.experiments.expF.WebsocketLoadableComponent
import mts.experiments.expF.HighlightingComponent

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.macros._
import monocle.std.option
import japgolly.scalajs.react.MonocleReact._

object ManualQAClient extends TaskClient[Unit, Unit] {

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[ManualQAApiRequest, ManualQAApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int, Int)]
  import HighlightingComponent._

  @Lenses case class State(
    recordsOpt: Option[List[ManualQARecord]],
    curRecord: Int,
    curGroup: Int,
    curQuestion: Int)

  def getAnswerSpan(curRecord: Int, curGroup: Int, curQuestion: Int, hs: HighlightingState): Set[Int] = hs.span.collect {
    case (rIndex, gIndex, qIndex, i) if rIndex == curRecord && gIndex == curGroup && qIndex == curQuestion => i
  }

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def processResponse(response: ManualQAApiResponse): Callback = response match {
      case AllRecordsResponse(records) => scope.modState(State.recordsOpt.set(Some(records)))
    }

    def updateRecords(s: State)(hs: HighlightingState) = scope.modState(
      (State.recordsOpt composePrism option.some).modify { records =>
        records.zipWithIndex.map {
          case (record, rIndex) => ManualQARecord(
            record.path, record.sentence,
            record.qaGroups.zipWithIndex.map {
              case (qaGroup, gIndex) =>
                qaGroup.zipWithIndex.map {
                  case (kqa, qIndex) =>
                    kqa.copy(answerIndices = getAnswerSpan(rIndex, gIndex, qIndex, hs))
                }
            }
          )

        }
      }
    )

    def qaGroup(s: State)(groupIndex: Int) = s match {
      case State(recordsOpt, curRecord, curGroup, curQuestion) =>
        val record = recordsOpt.get(curRecord)
        <.div(
          ^.border := "1px solid black",
          <.ul(
            Styles.listlessList,
            record.qaGroups(groupIndex).zipWithIndex.map {
              case (qaPair, qIndex) => qaField(s, qaPair, groupIndex, qIndex)
            }.map(field => <.li(^.display := "block", field)),
            <.li(
              <.input(
                Styles.bottomButton,
                ^.tabIndex := -1,
                ^.`type` := "button",
                ^.onClick --> scope.modState(
                  (State.recordsOpt composePrism option.some).modify { records =>
                    records.updated(
                      s.curRecord,
                      records(s.curRecord).copy(
                        qaGroups = records(s.curRecord).qaGroups.updated(
                          groupIndex,
                          records(s.curRecord).qaGroups(groupIndex) ++ List(KeywordedQAPair.blank)
                        )
                      )
                    )
                  }
                ),
                ^.value := "+")
            )
          )
        )
    }

    def qaField(s: State, qaPair: KeywordedQAPair, gIndex: Int, qIndex: Int) = {
      val isFocused = s.curGroup == gIndex && s.curQuestion == qIndex
      val answerIndices = qaPair.answerIndices
      val isAnswerEmpty = answerIndices.isEmpty
      val sentence = s.recordsOpt.get(s.curRecord).sentence
      val curAnswer = TextRendering.renderSentence(
        sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))
      val keywordIndex = s.recordsOpt.get(s.curRecord).qaGroups(gIndex)(qIndex).keywordIndex
      val curSpecialWordOpt =  if(keywordIndex == -1) {
        None
      } else {
        Some(TextRendering.normalizeToken(sentence.words(keywordIndex).token))
      }

      <.div(
        ^.overflow := "hidden",
        isFocused ?= Styles.greenBack,
        <.div(
          ^.overflow := "hidden",

          <.input(
            Styles.sideButton,
            ^.tabIndex := -1,
            ^.`type` := "button",
            ^.onClick --> scope.modState { s =>
              s match {
                case State(None, _, _, _) => s
                case State(Some(records), curRecord, curGroup, curQuestion) =>
                  State(
                    Some(records.updated(
                      s.curRecord,
                      records(s.curRecord).copy(
                        qaGroups =
                          if(records(s.curRecord).qaGroups(gIndex).size == 1) {
                            records(s.curRecord).qaGroups.remove(gIndex)
                          } else {
                            records(s.curRecord).qaGroups.updated(
                              gIndex,
                              records(s.curRecord).qaGroups(gIndex).remove(qIndex))
                          }
                      )
                    )),
                    curRecord,
                    if(gIndex == curGroup && qIndex == curQuestion && curQuestion == 0) curGroup - 1 else curGroup,
                    if(gIndex == curGroup && qIndex == curQuestion) {
                      if(curQuestion != 0) curQuestion - 1 else 0
                    } else curQuestion
                  )
              }

            },
            ^.value := "-"),
          <.input(
            ^.float := "left",
            ^.`type` := "text",
            ^.placeholder := s"Question",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "240px",
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  (State.recordsOpt composePrism option.some).modify { records =>
                    records.updated(
                      s.curRecord,
                      records(s.curRecord).copy(
                        qaGroups = records(s.curRecord).qaGroups.updated(
                          s.curGroup,
                          records(s.curRecord).qaGroups(s.curGroup).updated(
                            s.curQuestion,
                            records(s.curRecord).qaGroups(s.curGroup)(s.curQuestion).copy(
                              question = newValue
                            )
                          )
                        )))
                  })
              }),
            ^.onFocus --> (scope.modState(State.curGroup.set(gIndex)) >>
                             scope.modState(State.curQuestion.set(qIndex))),
            ^.value := qaPair.question
          ),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            curSpecialWordOpt
          )
        ),
        <.div(
          ^.overflow := "hidden",
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            isAnswerEmpty ?= (^.color := "#CCCCCC"),
            if(isAnswerEmpty && isFocused) {
              "Highlight your answer above"
            } else {
              curAnswer
            }
          )
        )
      )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri, request = AllRecordsRequest, onLoad = processResponse, render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(_, sendMessage) =>
              s.recordsOpt match {
                case None => <.div("Still loading records, hold on...")
                case Some(records) =>
                  val initHighlights = for {
                    (record, rIndex) <- records.zipWithIndex
                    (group, gIndex) <- record.qaGroups.zipWithIndex
                    (kqa, qIndex) <- group.zipWithIndex
                    aIndex <- kqa.answerIndices.toList
                  } yield (rIndex, gIndex, qIndex, aIndex)
                  import scalaz.std.list._
                  Highlighting(
                    HighlightingProps(
                      isEnabled = true, update = updateRecords(s),
                      initial = initHighlights.toSet, render = {
                        case (hs, HighlightingContext(startHighlight, startErase, stopHighlight, touchElement)) =>
                          val record = records(s.curRecord)
                          import record.sentence
                          val curSpan = getAnswerSpan(s.curRecord, s.curGroup, s.curQuestion, hs)
                          val curKeyword = records(s.curRecord).qaGroups(s.curGroup)(s.curQuestion).keywordIndex
                          def touchWord(i: Int)(e: ReactMouseEvent) =
                            if(e.shiftKey) {
                              // update keyword index to i
                              scope.modState(
                                (State.recordsOpt composePrism option.some).modify { records =>
                                  records.updated(
                                    s.curRecord,
                                    records(s.curRecord).copy(
                                      qaGroups = records(s.curRecord).qaGroups.updated(
                                        s.curGroup,
                                        records(s.curRecord).qaGroups(s.curGroup).updated(
                                          s.curQuestion,
                                          records(s.curRecord).qaGroups(s.curGroup)(s.curQuestion).copy(
                                            keywordIndex = i
                                          )
                                        )
                                      )))
                                })
                            } else {
                              touchElement((s.curRecord, s.curGroup, s.curQuestion, i))
                            }

                          <.div(
                            ^.onMouseUp --> stopHighlight,
                            ^.onMouseDown --> startHighlight,
                            Styles.mainContent,
                            <.div(
                              ^.position := "fixed",
                              ^.top := "0 px",
                              ^.left := "0 px",
                              ^.margin := "0 px",
                              ^.padding := "0 px",
                              ^.width := "100%",
                              ^.height := "150 px",
                              ^.backgroundColor := "white",
                              <.input(
                                ^.`type` := "button",
                                ^.onClick --> (
                                  sendMessage(AllRecordsUpdate(records)) >>
                                    scope.modState(
                                      State.curRecord.modify(i => (i + records.size - 1) % records.size) andThen
                                        State.curGroup.set(0) andThen State.curQuestion.set(0))
                                ),
                                ^.value := "prev"),
                              <.input(
                                ^.`type` := "button",
                                ^.onClick --> (
                                  sendMessage(AllRecordsUpdate(records)) >>
                                    scope.modState(
                                      State.curRecord.modify(i => (i + records.size + 1) % records.size) andThen
                                        State.curGroup.set(0) andThen State.curQuestion.set(0))
                                ),
                                ^.value := "next"),
                              <.p(
                                Styles.unselectable,
                                TextRendering.renderSentence(
                                  sentence.words,
                                  getToken = (word: CoNLLWord) => word.token,
                                  spaceFromNextWord = (nextWord: CoNLLWord) => List(
                                    <.span(
                                      ^.backgroundColor := (
                                        if(curSpan.contains(nextWord.index) && curSpan.contains(nextWord.index - 1)) {
                                          "#FFFF00"
                                        } else "transparent"),
                                      " ")),
                                  renderWord = (word: CoNLLWord) => List(
                                    <.span(
                                      ^.backgroundColor := (
                                        if(curSpan.contains(word.index)) "#FFFF00"
                                        else "transparent"),
                                      ^.onMouseMove ==> touchWord(word.index),
                                      ^.onMouseDown ==> (
                                        (e: ReactMouseEvent) => if(curSpan.contains(word.index)) {
                                          e.stopPropagation // so we don't trigger the global startHighlight
                                          startErase >> touchWord(word.index)(e)
                                        } else {
                                          startHighlight >> touchWord(word.index)(e)
                                        }
                                      ),
                                      (word.index == curKeyword) ?= Styles.niceBlue,
                                      TextRendering.normalizeToken(word.token)
                                    ))
                                ))
                            ),
                            <.div(
                              Styles.paddingTop150,
                              ^.overflow := "hidden",
                              <.ul(
                                Styles.listlessList,
                                (0 until record.qaGroups.size)
                                  .map(qaGroup(s))
                                  .map(field => <.li(^.display := "block", field))
                              ),
                              <.input(
                                Styles.bottomButton,
                                ^.tabIndex := -1,
                                ^.`type` := "button",
                                ^.onClick --> scope.modState(
                                  (State.recordsOpt composePrism option.some).modify { records =>
                                    records.updated(
                                      s.curRecord,
                                      records(s.curRecord).copy(
                                        qaGroups = records(s.curRecord).qaGroups ++ List(List(KeywordedQAPair.blank))
                                      ))}),
                                ^.value := "+")
                            )
                          )
                      }
                    )
                  )
              }
          }
        )
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(State(None, 0, 0, 0))
    .renderBackend[FullUIBackend]
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }
}
