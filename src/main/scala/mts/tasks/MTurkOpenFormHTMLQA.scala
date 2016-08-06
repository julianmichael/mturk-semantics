package mts.tasks

import mts.core.Question
import mts.qa.QASpec
import mts.qa.OpenFormHTMLQASpec

import scala.util.{Try, Success, Failure}

case class MTurkOpenFormHTMLQA(
  final override val reward: Double,
  final val numQAsPerHIT: Int,
  final override val numAssignmentsPerHIT: Int = 5
) extends MTurkTask {

  final override val title = s"Write $numQAsPerHIT questions and answers about a sentence"
  final override val description = s"""
    Read write $numQAsPerHIT questions about its meaning that are answered in the sentence.
  """.trim
  final override val keywords: String = "language,english,question answering"

  final override val qaSpec = OpenFormHTMLQASpec(numQAsPerHIT)
}
