package turksem.util

import turkey.tasks._

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
    sendMessage: Request => Callback) extends WebsocketLoadableState

  case class WebsocketLoadableProps(
    websocketURI: String,
    request: Request,
    onLoad: (Response => Callback) = (_ => Callback.empty),
    onMessage: (Response => Callback) = (_ => Callback.empty),
    render: (WebsocketLoadableState => ReactElement))

  class WebsocketLoadableBackend(scope: BackendScope[WebsocketLoadableProps, WebsocketLoadableState]) {
    def load(props: WebsocketLoadableProps): Callback = scope.state map {
      case Connecting =>
        val socket = new WebSocket(props.websocketURI)
        socket.onopen = { (event: Event) =>
          scope.setState(Loading).runNow
          socket.send(write(WebSocketMessage(props.request)))
        }
        socket.onerror = { (event: ErrorEvent) =>
          val msg = s"Connection failure. Error code: ${event.colno}"
          System.err.println(msg)
          // TODO maybe retry or something
        }
        socket.onmessage = { (event: MessageEvent) =>
          val msg = event.data.toString
          read[HeartbeatingWebSocketMessage[Response]](msg) match {
            case Heartbeat => socket.send(msg)
            case WebSocketMessage(response) => scope.modState {
              case Connecting =>
                System.err.println("Received message before socket opened?")
                props.onLoad(response).runNow
                Loaded(response, (r: Request) => Callback(socket.send(write[HeartbeatingWebSocketMessage[Request]](WebSocketMessage(r)))))
              case Loading =>
                props.onLoad(response).runNow
                Loaded(response, (r: Request) => Callback(socket.send(write[HeartbeatingWebSocketMessage[Request]](WebSocketMessage(r)))))
              case l @ Loaded(_, _) =>
                props.onMessage(response).runNow
                l
            }.runNow
          }
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