package mts.qa

import mts.core._

trait QASpec[QuestionData, AnswerData] {
  def createQuestion(qData: QuestionData): Question
  def extractQuestionData(q: Question): QuestionData
  def extractAnswerData(annotation: Annotation): AnswerData

  final def getQAPair(annotation: Annotation): Option[(QuestionData, AnswerData)] = for {
    question <- annotation.question
  } yield (extractQuestionData(question), extractAnswerData(annotation))
}
