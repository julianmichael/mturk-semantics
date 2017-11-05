package example.qpose

import turksem.gapfill.GapfillDispatcher
import turksem.gapfill.QuestionGuesser
// import turksem.gapfill.lowerCaseStringReader
// import turksem.gapfill.lowerCaseStringWriter

import example.emnlp2017.SentenceId

import turksem.util._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scalajs.js.JSApp

import upickle.default._

import turksem.gapfill.LowerCaseStringSerialization._

// very annoying that I have to do this...
import SentenceQGuesser.sentenceQGuesserReader
import SentenceQGuesser.sentenceQGuesserWriter

// need to pass these explicitly to avoid scala.js compiler stackoverflow. nice
object Dispatcher extends GapfillDispatcher[SentenceId, SentenceQGuesser] with JSApp {

  def instructions = <.div("Placeholder for instructions.")
}
