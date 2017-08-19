package turksem.qamr

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import japgolly.scalajs.react.vdom.html_<^.VdomTag

import upickle.default._

abstract class QAMRDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  def generationInstructions: VdomTag
  def requireWhAtQuestionBeginning: Boolean
  def validationInstructions: VdomTag

  lazy val genClient = new GenerationClient[SID](
    generationInstructions,
    requireWhAtQuestionBeginning,
    QAMRSettings)

  lazy val valClient = new ValidationClient[SID](
    validationInstructions,
    QAMRSettings)

  val dashClient: DashboardClient[SID] = new DashboardClient[SID](QAMRSettings)

  final override lazy val taskMapping = Map[String, () => Unit](
    QAMRSettings.expHGenerationTaskKey -> genClient.main,
    QAMRSettings.expHValidationTaskKey -> valClient.main,
    QAMRSettings.expHDashboardTaskKey -> dashClient.main)

}
