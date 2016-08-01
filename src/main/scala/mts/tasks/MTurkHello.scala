package mts.tasks

import mts.Question
import mts.qa.HelloSpec

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.service.exception.ServiceException
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.requester.HIT

import scala.util.{Try, Success, Failure}

object MTurkHello extends MTurkTask {

  override val title = "Answer a question"
  override val description = "This is a HIT created by the Mechanical Turk SDK.  Please answer the provided question."
  override val keywords: String = "test"
  override val reward = 0.05;

  override val numAssignmentsPerHIT: Int = 1

  override val qaSpec = HelloSpec

  val questions = List(
    "What is the weather like right now in Seattle, WA?",
    "What is the weather like right now in Austin, TX?",
    "What is the weather like right now in New York City?",
    "What is the weather like right now in Portland, OR?"
  )
}
