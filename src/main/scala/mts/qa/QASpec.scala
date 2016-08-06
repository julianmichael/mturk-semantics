package mts.qa

import mts.Question
import mts.Annotation

trait QASpec {
  type QuestionData
  type AnswerData

  def createQuestion(qData: QuestionData): Question
  def extractQuestionData(q: Question): QuestionData

  def extractAnswerData(answerXML: String): AnswerData

  final def getQAPair(annotation: Annotation): Option[(QuestionData, AnswerData)] = for {
    question <- annotation.question
  } yield (extractQuestionData(question), extractAnswerData(annotation.answer))
}
