package mts.experiments.sample

import mts.experiments._
import mts.tasks._

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

object Client extends TaskClient[SamplePrompt, SampleResponse] {

  sealed trait State
  @Lenses case class Loading(
    message: String
  ) extends State
  @Lenses case class Loaded(
    sentence: String,
    isGood: Boolean
  ) extends State
  object State {
    def loading[A]: Prism[State, Loading] = GenPrism[State, Loading]
    def loaded[A]: Prism[State, Loaded] = GenPrism[State, Loaded]
  }

  val isGoodLens = State.loaded composeLens Loaded.isGood

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def load: Callback = scope.state map {
      case Loading(_) =>
        val socket = new dom.WebSocket(websocketUri)
        socket.onopen = { (event: Event) =>
          scope.setState(Loading("Retrieving data")).runNow
          socket.send(write(SentenceRequest(prompt.id)))
        }
        socket.onerror = { (event: ErrorEvent) =>
          val msg = s"Connection failure. Error code: ${event.colno}"
          System.err.println(msg)
          // TODO maybe retry or something
        }
        socket.onmessage = { (event: MessageEvent) ⇒
          val response = read[ApiResponse](event.data.toString)
          response match {
            case SentenceResponse(id, sentence) =>
              scope.setState(Loaded(sentence, false)).runNow
          }
        }
        socket.onclose = { (event: Event) =>
          val msg = s"Connection lost."
          System.err.println(msg)
        }
      case Loaded(_, _) =>
        System.err.println("Data already loaded.")
    }

    def updateResponse: Callback = scope.state.map {
      st => isGoodLens.getOption(st).map(SampleResponse.apply).foreach(setResponse)
    }

    def render(s: State) = {
      <.div(
        Styles.mainContent,
        instructions,
        s match {
          case Loading(msg) =>
            <.p(s"Loading sentence ($msg)...")
          case Loaded(sentence, isGood) =>
            <.div(
              <.p(sentence),
              <.p(
                <.label(
                  <.input(
                    ^.`type` := "checkbox",
                    ^.checked := isGood,
                    ^.onChange --> scope.modState(isGoodLens.modify(!_))
                  ),
                  "Yes, it is a good sentence."
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
    .componentDidMount(context => context.backend.load)
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""This is a sample task. Please indicate whether the given sentence is good.
           Examples of good sentences include:"""),
    <.ul(
      <.li("""Why did you vote for Hillary when you knew she would send me to war?"""),
      <.li("""Tell her that a double-income family is actually the true Igbo tradition
           because in pre-colonial times, mothers farmed and traded."""),
      <.li("""Chudi does not deserve any special gratitude or praise, nor do you -
           you both made the choice to bring a child into the world,
           and the responsibility for that child belongs equally to you both.""")),
    <.p("""Examples of not-good sentences include:"""),
    <.ul(
      <.li("""So because of her unfounded concern over vote rigging, she committed voter fraud."""),
      <.li("""Make America great again."""),
      <.li("""Comey told FBI employees he didn't want to "be misleading to the American people"
           by not supplementing the record of the investigation."""),
      <.li("""Donald Trump and Vladimir Putin's bromance has been
           the weirdest subplot of America's wild presidential election.""")),
    <.hr(),
    <.p(s"""Please indicate whether the following sentence is good:""")
  )
}
