package example.interactive

import example.emnlp2017.SentenceId

import turksem.iqa._

import turkey.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import japgolly.scalajs.react.vdom.html_<^._

import upickle.default._

import scalajs.js.JSApp

object Dispatcher extends IQADispatcher[SentenceId] with JSApp {

  lazy val instructions: VdomTag = <.p("Placeholder for instructions.")
}
