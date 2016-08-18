package mts.experiments

import mts.conll.CoNLLSentencePath
import mts.language.tokenize

package object expB {

  // prompt and response data structures for HITs

  // requirement: path is the same as path in qas
  case class ValidationPrompt(
    path: CoNLLSentencePath,
    qas: List[ValidationQuestion])

  case class ValidationResponse(
    answers: List[ValidationAnswer],
    feedback: String)

  // general question/validation-holding data structures

  case class QAValidation(
    vQuestion: ValidationQuestion,
    vAnswers: List[ValidationAnswer])

  case class ValidationQuestion(
    path: CoNLLSentencePath,
    workerId: String,
    question: String,
    answer: String)

  sealed abstract class ValidationAnswer(val workerId: String) {
    def isValid = this match {
      case InvalidQuestion(_) => false
      case Answer(_, _) => true
    }

    def agreesExactlyWith(that: ValidationAnswer) = (this, that) match {
      case (InvalidQuestion(_), InvalidQuestion(_)) => true
      case (Answer(a1, _), Answer(a2, _)) => a1.equalsIgnoreCase(a2)
      case _ => false
    }

    def overlapsWith(that: ValidationAnswer) = (this, that) match {
      case (InvalidQuestion(_), InvalidQuestion(_)) => true
      case (Answer(a1, _), Answer(a2, _)) => {
        val a1Tokens = tokenize(a1.toLowerCase)
        val a2Tokens = tokenize(a2.toLowerCase).toSet
        a1Tokens.exists(a2Tokens)
      }
      case _ => false
    }
  }
  case class InvalidQuestion(override val workerId: String) extends ValidationAnswer(workerId) {
    override val toString = "N/A"
  }
  case class Answer(answer: String, override val workerId: String) extends ValidationAnswer(workerId) {
    override val toString = answer
  }
}
