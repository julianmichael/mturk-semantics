package mts.tasks

import mts.Question
import mts.qa.OpenFormMultipleQASpec

import scala.util.{Try, Success, Failure}

object MTurkOpenFormQA extends MTurkTask {

  override val title = "Write questions and answers about a sentence"
  override val description = """
    Read a sentence, and write questions about its meaning that are answered in the sentence.
  """.trim
  override val keywords: String = "test"
  override val reward = 0.05;

  override val numAssignmentsPerHIT: Int = 1

  override val qaSpec = OpenFormMultipleQASpec(3)
}
