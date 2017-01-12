package mts.experiments

import mts.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

abstract class TaskClient[Prompt : Reader, Response : Writer] {
  import scala.scalajs.js.Dynamic.global

  lazy val assignmentId: String = {
    global.turkSetAssignmentID()
    jQuery("#assignmentId").attr("value").get
  }

  lazy val taskKey: String = {
    read[String](jQuery(s"#$taskKeyLabel").attr("value").get)
  }

  lazy val serverDomain: String = {
    read[String](jQuery(s"#$serverDomainLabel").attr("value").get)
  }

  lazy val httpsPort: Int = {
    read[Int](jQuery(s"#$httpsPortLabel").attr("value").get)
  }

  lazy val websocketUri: String = {
    s"wss://$serverDomain:$httpsPort/websocket?taskKey=$taskKey"
  }

  lazy val prompt: Prompt = {
    read[Prompt](jQuery(s"#$promptLabel").attr("value").get)
  }

  lazy val externalSubmitURL: String = {
    jQuery(s"form#$mturkFormLabel").attr("action").get
  }

  def setResponse(response: Response): Unit = {
    jQuery(s"#$responseLabel").attr("value", write(response))
  }

  def main(): Unit
}
