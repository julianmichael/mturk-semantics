package turksem.scisrl.scisrlexample

import turksem.scisrl._
import turksem.util.dollarsToCents

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scalajs.js.JSApp

object SciSRLExampleDispatcher extends SciSRLDispatcher[SentenceId] with JSApp {

  import japgolly.scalajs.react.vdom.prefix_<^._
  import japgolly.scalajs.react._

  import scalacss.DevDefaults._
  import scalacss.ScalaCssReact._

  override val generationInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p("These are the task instructions."))

}
