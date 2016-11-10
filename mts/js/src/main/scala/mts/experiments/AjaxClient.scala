package mts.experiments

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom

import upickle.default._
import autowire._

object AjaxClient extends autowire.Client[String, Reader, Writer]{
  override def doCall(req: Request): Future[String] = {
    dom.ext.Ajax.post(
      url = "/ajax/" + req.path.mkString("/"),
      data = write(req.args.toSeq)
    ).map(_.responseText)
  }

  def read[Result : Reader](p: String) = read[Result](p)
  def write[Result : Writer](r: Result) = write(r)
}
