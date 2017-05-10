package mts

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

object MTSDispatcher extends TaskDispatcher with JSApp {

  override lazy val taskMapping = Map[String, () => Unit](
    expHGenerationTaskKey -> GenerationClient.main,
    expHValidationTaskKey -> ValidationClient.main,
    expHDashboardTaskKey -> DashboardClient.main)

}
