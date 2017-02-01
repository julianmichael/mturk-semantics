package mts.experiments

import mts.conll.CoNLLSentencePath
import mts.conll.CoNLLSentence

import expE.QAGenPrompt
import expE.QAGenResponse

package object expF extends PackagePlatformExtensions {

  case class SourcedQAPair(originalHITId: String, question: String, answer: Set[Int])

  case class ValidationPrompt(path: CoNLLSentencePath, sourcedQAPairs: List[SourcedQAPair])
  case class QuestionValidationResponse(questions: List[Option[String]])
  case class AnswerValidationResponse(answerIndices: List[Set[Int]])

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(sentence: CoNLLSentence) extends ApiResponse
}

