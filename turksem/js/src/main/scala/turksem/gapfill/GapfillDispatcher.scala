package turksem.gapfill

import spacro.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import japgolly.scalajs.react.vdom.html_<^._

import upickle.default._

import scalajs.js.JSApp

abstract class GapfillDispatcher[SID : Reader : Writer, ClientGuesser : Reader : Writer : QuestionGuesser] extends TaskDispatcher {

  def instructions: VdomTag

  import LowerCaseStringSerialization._

  lazy val client = new GapfillClient[SID, ClientGuesser](instructions)

  final override lazy val taskMapping = Map[String, () => Unit](
    gapfillTaskKey -> client.main)
}
