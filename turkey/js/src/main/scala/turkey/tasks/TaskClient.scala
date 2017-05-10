package turkey.tasks

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery
import org.scalajs.dom

import upickle.default._

abstract class TaskClient[Prompt : Reader, Response : Writer] {
  import scala.scalajs.js.Dynamic.global

  lazy val assignmentId: String = {
    global.turkSetAssignmentID()
    jQuery("#assignmentId").attr("value").get
  }

  lazy val isNotAssigned = assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE"

  lazy val taskKey: String = {
    read[String](jQuery(s"#$taskKeyLabel").attr("value").get)
  }

  lazy val serverDomain: String = {
    read[String](jQuery(s"#$serverDomainLabel").attr("value").get)
  }

  lazy val httpPort: Int = {
    read[Int](jQuery(s"#$httpPortLabel").attr("value").get)
  }

  lazy val httpsPort: Int = {
    read[Int](jQuery(s"#$httpsPortLabel").attr("value").get)
  }

  def getWebsocketUri(document: dom.Document, nameOfChatParticipant: String): String = {
    val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"
    s"$wsProtocol://${dom.document.location.host}/chat?name=$nameOfChatParticipant"
  }

  lazy val websocketUri: String = {
    val isHttps = dom.document.location.protocol == "https:"
    val wsProtocol = if (isHttps) "wss" else "ws"
    val serverPort = if(isHttps) httpsPort else httpPort
    s"$wsProtocol://$serverDomain:$serverPort/websocket?taskKey=$taskKey"
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

  // XXX get rid of this; we now put submit button where desired with React
  @Deprecated
  def setSubmitEnabled(enable: Boolean): Unit = {
    jQuery(s"#$submitButtonLabel").prop("disabled", !enable)
  }

  def main(): Unit
}
