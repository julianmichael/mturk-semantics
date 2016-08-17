package mts.experiments

import mts.conll.CoNLLSentencePath

package object expB {

  // requirement: path is the same as path in qas
  case class ValidationPrompt(
    path: CoNLLSentencePath,
    qas: List[ValidationQuestion])

  case class ValidationQuestion(
    path: CoNLLSentencePath,
    workerId: String,
    question: String,
    answer: String)

  case class ValidationResponse(
    answers: List[ValidationAnswer],
    feedback: String)

  sealed trait ValidationAnswer
  case object InvalidQuestion extends ValidationAnswer {
    override val toString = "N/A"
  }
  case class Answer(s: String) extends ValidationAnswer {
    override def toString = s
  }
}
