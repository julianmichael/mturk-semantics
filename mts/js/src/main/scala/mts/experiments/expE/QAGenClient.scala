package mts.experiments.expE

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import autowire._
import upickle.default._

import monocle._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

object QAGenClient extends TaskClient[QAGenPrompt, QAGenResponse] {

  sealed trait HighlightingState
  case object DoingNothing extends HighlightingState
  case object Highlighting extends HighlightingState
  case object Erasing extends HighlightingState

  sealed trait State
  @Lenses case class Loading(
    message: String
  ) extends State
  @Lenses case class Loaded(
    sentence: CoNLLSentence,
    qaPairs: List[(String, Set[Int])],
    currentFocus: Int,
    highlightingState: HighlightingState
  ) extends State {
    def questionWord = sentence.words(prompt.wordIndex)
  }
  object State {
    def loading[A]: Prism[State, Loading] = GenPrism[State, Loading]
    def loaded[A]: Prism[State, Loaded] = GenPrism[State, Loaded]
  }

  val currentFocusLens = State.loaded composeLens Loaded.currentFocus
  val qaPairsLens = State.loaded composeLens Loaded.qaPairs
  val highlightingStateLens = State.loaded composeLens Loaded.highlightingState

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def load: Callback = scope.state map {
      case Loading(_) =>
        val socket = new dom.WebSocket(websocketUri)
        socket.onopen = { (event: Event) =>
          scope.setState(Loading("Retrieving data")).runNow
          socket.send(write(SentenceRequest(prompt.path)))
        }
        socket.onerror = { (event: ErrorEvent) =>
          val msg = s"Connection failure. Error code: ${event.colno}"
          System.err.println(msg)
          // TODO maybe retry or something
        }
        socket.onmessage = { (event: MessageEvent) ⇒
          val response = read[ApiResponse](event.data.toString)
          response match {
            case SentenceResponse(path, sentence) =>
              scope.setState(Loaded(sentence, List(("", Set.empty[Int])), 0, DoingNothing)).runNow
          }
        }
        socket.onclose = { (event: Event) =>
          val msg = s"Connection lost."
          System.err.println(msg)
          // TODO maybe retry or something
        }
      case Loaded(_, _, _, _) =>
        System.err.println("Data already loaded.")
    }

    def updateResponse: Callback = scope.state.map {
      st => qaPairsLens.getOption(st).map(QAGenResponse.apply).foreach(setResponse)
    }

    def qaField(loadedState: Loaded, index: Int) = loadedState match {
      case ls @ Loaded(sentence, qaPairs, currentFocus, _) =>
        <.p(
          <.input(
            ^.`type` := "text",
            ^.required := index == 0,
            ^.placeholder := s"""Use "${ls.questionWord.token}" in a short question""",
            ^.margin := "1 px",
            ^.padding := "1 px",
            ^.width := 240,
            ^.value := qaPairs(index)._1,
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  qaPairsLens.modify(
                    qaPairs => {
                      val currentQA = qaPairs(index)
                      val newQA = currentQA.copy(_1 = newValue)
                      qaPairs.updated(index, newQA)
                    }))
              }),
            ^.onFocus --> scope.modState(currentFocusLens.set(index))
          ),
          <.span(
            ^.id := s"answer-$index",
            ^.margin := "1 px",
            ^.padding := "1 px",
            TextRendering.renderSentence(sentence.words.filter(w => qaPairs(index)._2.contains(w.index)).map(_.token))
          )
        )
    }

    def render(s: State) = {

      <.div(
        ^.onMouseUp --> scope.modState(highlightingStateLens.set(DoingNothing)),
        ^.onMouseDown --> scope.modState(highlightingStateLens.set(Highlighting)),
        Styles.mainContent,
        instructions,
        s match {
          case Loading(msg) =>
            <.p(s"Loading sentence ($msg)...")
          case ls @ Loaded(sentence, qaPairs, currentFocus, highlightingState) =>
            val answerSpans = qaPairs.map(_._2)
            val curAnswer = answerSpans(currentFocus)
            import scalaz.std.list._
            <.div(
              <.p(
                Styles.unselectable,
                TextRendering.renderSentence(
                  sentence.words,
                  (word: CoNLLWord) => word.token,
                  (nextWord: CoNLLWord) => List(
                    <.span(
                      ^.backgroundColor := (
                        if(curAnswer.contains(nextWord.index) && curAnswer.contains(nextWord.index - 1)) {
                          "#FFFF00"
                        } else if(!curAnswer.contains(nextWord.index) &&
                                    !curAnswer.contains(nextWord.index - 1) &&
                                    answerSpans.flatten.contains(nextWord.index) &&
                                    answerSpans.flatten.contains(nextWord.index - 1)) {
                          "#888888"
                        } else {
                          "transparent"
                        }),
                      " ")),
                  (word: CoNLLWord) => List(
                    <.span(
                      ^.backgroundColor := (
                        if(word.index == prompt.wordIndex) {
                          "#00FF00"
                        } else if(curAnswer.contains(word.index)) {
                          "#FFFF00"
                        } else if(answerSpans.flatten.contains(word.index)) {
                          "#888888"
                        } else {
                          "transparent"
                        }
                      ),
                      ^.onMouseMove --> (
                        highlightingState match {
                          case DoingNothing => Callback.empty
                          case Highlighting =>
                            if(!curAnswer.contains(word.index) && word.index != prompt.wordIndex) {
                              scope.modState(
                                qaPairsLens.modify(
                                  qaPairs => {
                                    val currentQA = qaPairs(currentFocus)
                                    val newQA = currentQA.copy(_2 = currentQA._2 + word.index)
                                    qaPairs.updated(currentFocus, newQA)
                                  }))
                            } else Callback.empty
                          case Erasing =>
                            if(curAnswer.contains(word.index)) {
                              scope.modState(
                                qaPairsLens.modify(
                                  qaPairs => {
                                    val currentQA = qaPairs(currentFocus)
                                    val newQA = currentQA.copy(_2 = currentQA._2 - word.index)
                                    qaPairs.updated(currentFocus, newQA)
                                  }))
                            } else Callback.empty
                        }),
                      ^.onMouseDown ==> (
                        (e: ReactEventI) => if(curAnswer.contains(word.index)) {
                          e.stopPropagation
                          scope.modState(highlightingStateLens.set(Erasing))
                        } else {
                          scope.modState(highlightingStateLens.set(Highlighting))
                        }
                      ),
                      TextRendering.normalizeToken(word.token)
                    ))
                )),
              <.ul(
                (0 until qaPairs.size).map(i =>
                  <.li(qaField(ls, i))
                )
              )
            )
        }
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(Loading("Connecting to server"): State)
    .renderBackend[FullUIBackend]
    .componentDidMount(
    context => {
      context.backend.load
    })
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""Instructions go here.
           """))
}
