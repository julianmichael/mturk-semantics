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
    // this needs to be done in order for the form submit to work
    global.turkSetAssignmentID()
    // dispatch to specific task
    taskKey match {
      case TaskIndex.`sampleTaskKey` => sample.Client.main()
      case TaskIndex.`expEQAGenTaskKey` => expE.QAGenClient.main()
      case TaskIndex.`expFAnswerValidationTaskKey` => expF.AnswerValidationClient.main()
      case TaskIndex.`expFQuestionValidationTaskKey` => expF.QuestionValidationClient.main()
      case TaskIndex.`expFLongAnswerValidationTaskKey` => expF.LongAnswerValidationClient.main()
      case TaskIndex.`expGWordLimQValTaskKey` => expG.WordLimitingQuestionValidationClient.main()
      case TaskIndex.`expGManualQATaskKey` => expG.ManualQAClient.main()
      case TaskIndex.`expHGenerationTaskKey` => expH.GenerationClient.main()
      case TaskIndex.`expHValidationTaskKey` => expH.ValidationClient.main()
      // case _ => // this will happen if it couldn't connect over the network...wait...no? idk
      //   jQuery(s"#$rootClientDivLabel").append(
      //     "Could not connect to the server"
      //   )
    }
  }
}
