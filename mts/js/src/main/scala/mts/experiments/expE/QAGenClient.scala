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
    currentSpan: Set[Int],
    highlightingState: HighlightingState
  ) extends State
  object State {
    def loading[A]: Prism[State, Loading] = GenPrism[State, Loading]
    def loaded[A]: Prism[State, Loaded] = GenPrism[State, Loaded]
  }

  val qaPairLens = State.loaded composeLens Loaded.qaPairs

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
              scope.setState(Loaded(sentence, Nil, 0, Set.empty[Int], DoingNothing)).runNow
          }
        }
        socket.onclose = { (event: Event) =>
          val msg = s"Connection lost."
          System.err.println(msg)
          // TODO maybe retry or something
        }
      case Loaded(_, _, _, _, _) =>
        System.err.println("Data already loaded.")
    }

    def updateResponse: Callback = scope.state.map {
      st => qaPairLens.getOption(st).map(QAGenResponse.apply).foreach(setResponse)
    }

    def render(s: State) = {
      <.div(
        Styles.mainContent,
        instructions,
        s match {
          case Loading(msg) =>
            <.p(s"Loading sentence ($msg)...")
          case Loaded(sentence, qaPairs, currentFocus, currentSpan, highlightingState) =>
            <.div(
              <.p(TextRendering.renderSentence(sentence)),
              <.ul(
                <.li("To do: Question fields and highlighting")
              )
            )
        }
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(Loading("Connecting to server"): State)
    .renderBackend[FullUIBackend]
    .componentDidMount(context => context.backend.load)
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  // QA specification methods

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""Instructions go here.
           """))

  // private[this] final def makeQAElement = {
  //   <.p(
  //     ^.margin := 0,
  //     ^.padding := 0
  //   )(
  //     <.input(
  //       ^.`type` := "text",
  //       ^.required,
  //       ^.name := s"question-$questionNum",
  //       ^.placeholder := "Question",
  //       ^.margin := 1,
  //       ^.padding := 1,
  //       ^.width := 240,
  //       ^.font := pageFont
  //     ),
  //     <.input(
  //       ^.`type` := "text",
  //       ^.required,
  //       ^.name := s"answer-$questionNum",
  //       ^.placeholder := "Answer",
  //       ^.`class` := "answerField",
  //       ^.margin := 1,
  //       ^.padding := 1,
  //       ^.width := 240,
  //       ^.font := pageFont
  //     )
  //   )
  // }
}
