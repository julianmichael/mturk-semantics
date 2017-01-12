package mts.experiments

import mts.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

object TaskDispatcher extends JSApp {
  import scala.scalajs.js.Dynamic.global

  lazy val taskKey: String = {
    read[String](jQuery(s"#$taskKeyLabel").attr("value").get)
  }

  final def main(): Unit = jQuery { () =>
    // dispatch to specific task
    taskKey match {
      case "sample" => sample.Client.main()
    }
  }
}
