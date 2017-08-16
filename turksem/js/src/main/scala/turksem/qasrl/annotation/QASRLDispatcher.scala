package turksem.qasrl.annotation

import turksem.qasrl._

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

import japgolly.scalajs.react.vdom.prefix_<^.ReactTag

abstract class QASRLDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  def generationInstructions: ReactTag
  def validationInstructions: ReactTag

  lazy val genClient = new QASRLGenerationClient[SID](
    generationInstructions,
    QASRLSettings)

  lazy val valClient = new turksem.qamr.annotation.ValidationClient[SID](
    validationInstructions,
    QASRLSettings)

  val dashClient: turksem.qamr.annotation.DashboardClient[SID] =
    new turksem.qamr.annotation.DashboardClient[SID](QASRLSettings)

  final override lazy val taskMapping = Map[String, () => Unit](
    QASRLSettings.generationTaskKey -> genClient.main,
    QASRLSettings.validationTaskKey -> valClient.main,
    QASRLSettings.dashboardTaskKey -> dashClient.main)

}
