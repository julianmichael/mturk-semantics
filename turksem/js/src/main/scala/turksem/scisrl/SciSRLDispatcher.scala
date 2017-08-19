package turksem.scisrl

import turkey.tasks.TaskDispatcher

import upickle.default._

import japgolly.scalajs.react.vdom.html_<^.VdomTag

/**
  * Forms the main class for the annotation project, and
  * links up the "task keys" to their associated client code.
  */
abstract class SciSRLDispatcher[SID : Reader : Writer] extends TaskDispatcher {

  def generationInstructions: VdomTag

  lazy val genClient = new SciSRLClient[SID](generationInstructions)

  final override lazy val taskMapping = Map[String, () => Unit](
    SciSRLSettings.generationTaskKey -> genClient.main)

}
