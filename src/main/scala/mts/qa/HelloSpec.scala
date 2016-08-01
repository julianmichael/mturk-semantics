package mts.qa

import mts.Question
import mts.Annotation

import com.amazonaws.mturk.service.axis.RequesterService

object HelloSpec extends QASpec {
  type QuestionData = String
  type AnswerData = String

  override def createQuestion(qData: QuestionData): Question =
    Question(RequesterService.getBasicFreeTextQuestion(qData), qData)
  override def extractQuestionData(q: Question): QuestionData =
    q.annotation

  override def extractAnswerData(answerXML: String): AnswerData = {
    val openTag = "<FreeText>"
    val closeTag = "</FreeText>"
    answerXML.substring(answerXML.indexOf(openTag) + openTag.length, answerXML.indexOf(closeTag))
  }
}
