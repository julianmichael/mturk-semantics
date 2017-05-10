package turkey.tasks

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

trait TaskDispatcher {

  // override this with the mapping from your task key to your task's main method
  def taskMapping: Map[String, () => Unit]

  import scala.scalajs.js.Dynamic.global

  lazy val taskKey: String = {
    read[String](jQuery(s"#$taskKeyLabel").attr("value").get)
  }

  final def main(): Unit = jQuery { () =>
    // this needs to be done in order for the form submit to work
    global.turkSetAssignmentID()
    // dispatch to specific task
    taskMapping.get(taskKey) match {
      case None => System.err.println(s"Invalid task key: $taskKey")
      case Some(func) => func()
    }
  }
}
