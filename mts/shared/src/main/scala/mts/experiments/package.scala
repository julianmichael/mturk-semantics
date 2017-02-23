package mts

package object experiments extends PackagePlatformExtensions {
  sealed trait HeartbeatingWebSocketMessage[+A]
  case object Heartbeat extends HeartbeatingWebSocketMessage[Nothing]
  case class WebSocketMessage[A](content: A) extends HeartbeatingWebSocketMessage[A]
}
