package mts.emnlp2017

import mts.annotation.QAMRDispatcher
import scalajs.js.JSApp

object Dispatcher extends QAMRDispatcher[SentenceId] with JSApp
