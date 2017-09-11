package turksem.qasrl

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

import japgolly.scalajs.react.vdom.html_<^.VdomTag

abstract class QASRLDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  def generationPreTaskInstructions: VdomTag
  def generationPostTaskInstructions: VdomTag
  def validationPreTaskInstructions: VdomTag
  def validationPostTaskInstructions: VdomTag

  lazy val genClient = new QASRLGenerationClient[SID](
    generationPreTaskInstructions, generationPostTaskInstructions)

  lazy val valClient = new QASRLValidationClient[SID](
    validationPreTaskInstructions, validationPostTaskInstructions)

  // val dashClient: turksem.qamr.DashboardClient[SID] =
  //   new turksem.qamr.DashboardClient[SID](QASRLSettings)

  final override lazy val taskMapping = Map[String, () => Unit](
    QASRLSettings.generationTaskKey -> genClient.main,
    QASRLSettings.validationTaskKey -> valClient.main//,
    /*QASRLSettings.dashboardTaskKey -> dashClient.main*/)

}
