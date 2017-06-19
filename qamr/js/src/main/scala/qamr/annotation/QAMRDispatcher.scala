package qamr
package annotation

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

abstract class QAMRDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  def genClient: GenerationClient[SID]
  def valClient: ValidationClient[SID]
  val dashClient: DashboardClient[SID] = new DashboardClient[SID]

  final override lazy val taskMapping = Map[String, () => Unit](
    expHGenerationTaskKey -> genClient.main,
    expHValidationTaskKey -> valClient.main,
    expHDashboardTaskKey -> dashClient.main)

}
