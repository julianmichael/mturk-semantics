package mts.experiments.expF

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._
import mts.util.dollarsToCents

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.WebSocket
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

import upickle.default._

class WebsocketLoadableComponent[Request : Writer, Response : Reader] {

  sealed trait WebsocketLoadableState
  case object Connecting extends WebsocketLoadableState
  case object Loading extends WebsocketLoadableState
  case class Loaded(
    content: Response,
    socket: WebSocket) extends WebsocketLoadableState

  case class WebsocketLoadableProps(
    websocketURI: String,
    request: Request,
    render: (WebsocketLoadableState => ReactElement))

  class WebsocketLoadableBackend(scope: BackendScope[WebsocketLoadableProps, WebsocketLoadableState]) {
    def load(props: WebsocketLoadableProps): Callback = scope.state map {
      case Connecting =>
        val socket = new WebSocket(props.websocketURI)
        socket.onopen = { (event: Event) =>
          scope.setState(Loading).runNow
          socket.send(write(props.request))
        }
        socket.onerror = { (event: ErrorEvent) =>
          val msg = s"Connection failure. Error code: ${event.colno}"
          System.err.println(msg)
          // TODO maybe retry or something
        }
        socket.onmessage = { (event: MessageEvent) =>
          scope.setState(Loaded(read[Response](event.data.toString), socket)).runNow
        }
        socket.onclose = { (event: Event) =>
          val msg = s"Connection lost."
          System.err.println(msg)
          // TODO maybe retry or something. probably not. right now this always happens because no heartbeat
        }
      case Loading =>
        System.err.println("Data already loading.")
      case Loaded(_, _) =>
        System.err.println("Data already loaded.")
    }

    def render(props: WebsocketLoadableProps, s: WebsocketLoadableState) =
      props.render(s)
  }

  val WebsocketLoadable = ReactComponentB[WebsocketLoadableProps]("Websocket Loadable")
    .initialState(Connecting: WebsocketLoadableState)
    .renderBackend[WebsocketLoadableBackend]
    .componentDidMount(context => context.backend.load(context.props))
    .build
}
