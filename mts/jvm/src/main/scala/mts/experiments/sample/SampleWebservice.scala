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
import akka.stream.Materializer
import akka.stream.scaladsl.Flow

import upickle.default._

object Router extends autowire.Server[String, Reader, Writer] {
  def read[Result : Reader](p: String) = read[Result](p)
  def write[Result : Writer](r: Result) = write(r)
}

object SampleAjaxApi extends SampleApi {
  override def getCoNLLSentence(path: CoNLLSentencePath): CoNLLSentence =
    FileManager.getCoNLLSentence(path).get
}

class SampleWebservice(implicit config: TaskConfig) extends Directives {
  // TODO verify that we're getting a JS file. don't just serve anything they ask for
  def route = getFromResourceDirectory("") ~
    post {
      path("ajax" / Segments) { s =>
        entity(as[String]) { e =>
          complete {
            Router.route[SampleApi](SampleAjaxApi)(
              autowire.Core.Request(s, read[Map[String, String]](e))
            )
          }
        }
      }
    }
}
