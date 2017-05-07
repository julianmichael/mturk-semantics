package mts.experiments

import nlpdata.datasets.conll._
import mts.util._
import mts.tasks._

import java.util.Date

import akka.actor.ActorSystem
import akka.stream.stage._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws.{ Message, TextMessage, BinaryMessage }
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl._

import upickle.default._

class Webservice(
  tasks: List[TaskSpecification])(
  implicit fm: Materializer,
  config: TaskConfig) extends Directives {
  // TODO verify that we're getting a JS file. don't just serve anything they ask for

  // assume keys are unique
  val taskIndex = tasks.map(t => (t.taskKey -> t)).toMap

  def route = get {
    pathSingleSlash {
      parameter('taskKey) { taskKey =>
        rejectEmptyResponse {
          complete {
            taskIndex.get(taskKey) map { taskSpec =>
              HttpEntity(
                ContentTypes.`text/html(UTF-8)`,
                TaskPage.htmlPage(taskSpec.samplePrompt, taskSpec, useHttps = false)(taskSpec.promptWriter, config).render
              )
            }
          }
        }
      }
    } ~ path("websocket") {
      parameter('taskKey) { taskKey =>
        handleWebSocketMessages(websocketFlow(taskKey))
      }
    }
  } ~ getFromResourceDirectory("")

  def websocketFlow(taskKey: String): Flow[Message, Message, Any] = {
    val taskOpt = taskIndex.get(taskKey)
    taskOpt match {
      case None =>
        System.err.println(s"Got API request for task $taskKey which matches no task")
        Flow[Message].filter(_ => false)
      case Some(taskSpec) =>
        import taskSpec._ // to get ApiRequest and ApiResponse types and serialization objects
        Flow[Message].map {
          case TextMessage.Strict(msg) =>
            Future.successful(List(read[HeartbeatingWebSocketMessage[ApiRequest]](msg)))
          case TextMessage.Streamed(stream) => stream
              .limit(10000)                 // Max frames we are willing to wait for
              .completionTimeout(5 seconds) // Max time until last frame
              .runFold("")(_ + _)           // Merges the frames
              .flatMap(msg => Future.successful(List(read[HeartbeatingWebSocketMessage[ApiRequest]](msg))))
          case bm: BinaryMessage =>
            // ignore binary messages but drain content to avoid the stream being clogged
            bm.dataStream.runWith(Sink.ignore)
            Future.successful(Nil)
        }.mapAsync(parallelism = 3)(identity).mapConcat(identity)
          .collect { case WebSocketMessage(request) => request } // ignore heartbeats
          .via(taskSpec.apiFlow) // this is the key line that delegates to task-specific logic
          .map(WebSocketMessage(_): HeartbeatingWebSocketMessage[ApiResponse])
          .keepAlive(30 seconds, () => Heartbeat) // send heartbeat every 30 seconds
          .map(message => TextMessage.Strict(write[HeartbeatingWebSocketMessage[ApiResponse]]((message))))
    }
  }
}
