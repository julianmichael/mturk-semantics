package mts.qa

import mts.core._
import mts.util._
import mts.conll._
import mts.tasks.Config

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

case class OpenFormHTMLQASpec(numQAs: Int) extends QASpec {
  type QuestionData = (CoNLLSentencePath, String) // path to sentence, sentence
  type AnswerData = (List[(String, String)], String) // QA pairs, comment

  final val pageFont = "14px Helvetica"

  final def makeQAElement(questionNum: Int) = {
    import scalatags.Text.all._
    p(
      margin := 0,
      padding := 0
    )(
      input(
        `type` := "text",
        required,
        name := s"question-$questionNum",
        placeholder := "Question",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      ),
      input(
        `type` := "text",
        required,
        name := s"answer-$questionNum",
        placeholder := "Answer",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      )
    )
  }

  final def createQuestion(qData: QuestionData): Question = {
    import scalatags.Text.all._
    val page = html(
      head(
        script(
          `type` := "text/javascript",
          src := "https://s3.amazonaws.com/mturk-public/externalHIT_v1.js"
        ),
        script(
          `type` := "text/javascript",
          src := "https://ajax.googleapis.com/ajax/libs/jquery/3.1.0/jquery.min.js"
        )
      ),
      body(
        font := pageFont
      )(
        p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
          We wish to deconstruct the meanings of English sentences into a list of questions and answers.
          You will be presented with an English sentence.
          Your task is to write $numQAs simple questions, and their answers, satisfying the following criteria:"""),
        ul(
          li("""The question must be about the meaning of the sentence, and not, for example, the positions of the words."""),
          li("""The question must contain a content word (such as a name, descriptor, or verb other than "be" or "do"),
             or a derivative of such a word, from the sentence."""),
          li("""The answer must only contain words (or derivatives of words) that appear in the sentence.""")),
        p("""Consider the sentence: "If the UK is unwilling to accept the free movement of labour,
          it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person."
          Acceptable questions and answers may include:"""),
        ul(
          li("""Who might be unwilling to do something? --- the UK"""),
          li("""What would lead to a decrease in income? --- trade falling"""),
          li("""How much of a decrease? --- 2.6 per cent""")),
        p("""
          Note that the answers only contain words (the, UK, trade, 2.6, per, cent) or derivatives of words (falling)
          that already appear in the sentence.
          Please try to keep the questions and answers as short as possible while remaining correct.
          Your goal should be that if someone else reads these instructions, the sentence, and your question,
          they are likely to write your answer word-for-word.
          If your response has questions that are all identical, do not use words from the sentence,
          or are clearly not English, it will be rejected.
          Otherwise, your work will be approved in at most one hour.
          Each HIT should take less than two minutes to complete."""),
        hr(),
        p(s"""Please write $numQAs questions and answers about the following sentence:"""),
        blockquote(qData._2),
        form(
          name := "mturk_form",
          method := "post",
          id := "mturk_form",
          action := Config.externalSubmitURL)(
          input(
            `type` := "hidden",
            value := "",
            name := "assignmentId",
            id := "assignmentId"),
          (1 to numQAs).map(makeQAElement),
          p(
            input(
              `type` := "text",
              name := s"comments",
              placeholder := "Feedback? (Optional)",
              margin := 1,
              padding := 1,
              width := 484,
              font := pageFont
            )
          ),
          input(
            `type` := "submit",
            id := "submitButton",
            value := "submit")),
        script(
          `type` := "text/javascript")("""
            turkSetAssignmentID();
            if($('#assignmentId').attr('value') === 'ASSIGNMENT_ID_NOT_AVAILABLE') {
              $('input').attr('readonly', true)
            }
          """)
      )
    )
    val question = s"""
      <?xml version="1.0" encoding="UTF-8"?>
      <HTMLQuestion xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd">
        <HTMLContent><![CDATA[
          <!DOCTYPE html>${page.render}
        ]]></HTMLContent>
        <FrameHeight>540</FrameHeight>
      </HTMLQuestion>
    """.trim
    Question(question, upickle.write(qData._1))
  }

  final def extractQuestionData(q: Question): QuestionData = {
    val path = upickle.read[CoNLLSentencePath](q.annotation)
    val sentence = FileManager.getCoNLLSentence(path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)
    (path, sentenceString)
  }

  final def extractAnswerData(answerXML: String): AnswerData = {
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
    val qaPairs = (0 until answers.size / 2)
      .map(_ + 1)
      .map(i => (answers.find(a => a._1.equals(s"question-$i")).get._2,
                 answers.find(a => a._1.equals(s"answer-$i")).get._2))
      .toList
    val comment = answers.find(a => a._1.equals("comments")).get._2
    (qaPairs, comment)
  }
}
