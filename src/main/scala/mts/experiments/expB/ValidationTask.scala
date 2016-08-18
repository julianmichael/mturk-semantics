package mts.experiments.expB

import mts.tasks._

import scala.util.{Try, Success, Failure}

case class ValidationTask(
  final override val numAssignmentsPerHIT: Int = 5
) extends MTurkTask[ValidationPrompt, ValidationResponse] {

  final override val reward = 0.10
  final override val title = s"Answer questions about a sentence"
  final override val description = s"""
    Evaluate and answer several questions a sentence's meaning.
  """.trim
  final override val keywords: String = "language,english,question answering"

  final override val qaSpec = ValidationQASpec
}
