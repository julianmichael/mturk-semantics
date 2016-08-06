package mts.tasks

import mts.Question
import mts.qa.QASpec
import mts.qa.OpenFormHTMLQASpec

import scala.util.{Try, Success, Failure}

case class MTurkOpenFormHTMLQA(
  final override val reward: Double,
  final val numQAsPerHIT: Int,
  final override val numAssignmentsPerHIT: Int = 5
) extends MTurkTask {
  // HIT type fields
  final override val title = "Write questions and answers about a sentence"
  final override val description = """
    Read a sentence, and write questions about its meaning that are answered in the sentence.
  """.trim
  final override val keywords: String = "language,english,question answering"

  final override val qaSpec = OpenFormHTMLQASpec(numQAsPerHIT)
}
