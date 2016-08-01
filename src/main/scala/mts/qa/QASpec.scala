package mts.qa

import mts.Question
import mts.Annotation

trait QASpec {
  type QuestionData
  type AnswerData

  def createQuestion(qData: QuestionData): Question
  def extractQuestionData(q: Question): QuestionData

  def extractAnswerData(answerXML: String): AnswerData

  final def getQAPair(annotation: Annotation): (QuestionData, AnswerData) = {
    (extractQuestionData(annotation.question), extractAnswerData(annotation.answer))
  }
}

class OpenFormMultipleQASpec(numQAs: Int = 5) {
  type QuestionData = String // sentence to annotate
  type AnswerData = List[(String, String)]

  def createQuestion(qData: QuestionData): Question
  def extractQuestionData(q: Question): QuestionData

  def extractAnswerData(answerXML: String): AnswerData

  final def getQAPair(annotation: Annotation): (QuestionData, AnswerData) = {
    (extractQuestionData(annotation.question), extractAnswerData(annotation.answer))
  }
}
