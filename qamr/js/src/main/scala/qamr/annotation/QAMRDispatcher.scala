package qamr
package annotation

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

class QAMRDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  lazy val genClient = new GenerationClient[SID]
  lazy val valClient = new ValidationClient[SID]
  lazy val dashClient = new DashboardClient[SID]

  override lazy val taskMapping = Map[String, () => Unit](
    expHGenerationTaskKey -> genClient.main,
    expHValidationTaskKey -> valClient.main,
    expHDashboardTaskKey -> dashClient.main)

}
