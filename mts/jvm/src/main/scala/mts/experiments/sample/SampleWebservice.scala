package mts.experiments.sample

import mts.conll._
import mts.util._
import mts.tasks.TaskConfig

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

class SampleWebservice(implicit fm: Materializer, config: TaskConfig) extends Directives {
  // TODO verify that we're getting a JS file. don't just serve anything they ask for
  def route = getFromResourceDirectory("") ~
    get {
      path("websocket") {
        parameter('assignmentId) { assignmentId =>
          handleWebSocketMessages(websocketFlow(assignmentId))
        }
      }
    }

  def websocketFlow(assignmentId: String): Flow[Message, Message, Any] =
    Flow[Message].map {
      case TextMessage.Strict(msg) =>
        Future.successful(List(read[ApiRequest](msg)))
      case TextMessage.Streamed(stream) => stream
          .limit(100)                   // Max frames we are willing to wait for
          .completionTimeout(5 seconds) // Max time until last frame
          .runFold("")(_ + _)           // Merges the frames
          .flatMap(msg => Future.successful(List(read[ApiRequest](msg))))
      case bm: BinaryMessage =>
        // ignore binary messages but drain content to avoid the stream being clogged
        bm.dataStream.runWith(Sink.ignore)
        Future.successful(Nil)
      }
      .mapAsync(parallelism = 3)(identity)
      .mapConcat(identity)
      .map {
      case SentenceRequest(path) => SentenceResponse(path, getCoNLLSentence(path)): ApiResponse
    }
      .map {
      case msg: ApiResponse => TextMessage.Strict(write(msg))
    }

  def getCoNLLSentence(path: CoNLLSentencePath): CoNLLSentence =
    FileManager.getCoNLLSentence(path).get
}
