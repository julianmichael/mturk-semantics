package mts.experiments.expA

import mts.core._
import mts.util._
import mts.conll._
import mts.tasks._

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import scala.util.{Try, Success, Failure}

case class OpenFormTask(
  final override val reward: Double,
  final val numQAs: Int,
  final override val numAssignmentsPerHIT: Int = 5
) extends MTurkTask[OpenFormPrompt, OpenFormResponse] {

  final override val title = s"Write $numQAs questions and answers about a sentence"
  final override val description = s"""
    Read write $numQAs questions about its meaning that are answered in the sentence.
  """.trim
  final override val keywords: String = "language,english,question answering"

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

  final def createQuestion(qData: OpenFormPrompt): Question = {
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
          You will be presented with a selection of English text, usually a sentence or phrase.
          Your task is to write $numQAs simple questions, and their answers, satisfying the following criteria:"""),
        ul(
          li("""The question must be about the meaning of the selection, and not, for example, the positions of the words."""),
          li("""The question must contain a content word (such as a name, descriptor, or verb other than "be" or "do"),
             or a derivative of such a word, from the selection."""),
          li("""The answer must only contain words (or derivatives of words) that appear in the selection.""")),
        p("""Consider the sentence: "If the UK is unwilling to accept the free movement of labour,
          it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person."
          Acceptable questions and answers may include:"""),
        ul(
          li("""Who might be unwilling to do something? --- the UK"""),
          li("""What would lead to a decrease in income? --- trade falling"""),
          li("""How much of a decrease? --- 2.6 per cent""")),
        p(s"""
          Note that the answers only contain words (the, UK, trade, 2.6, per, cent) or derivatives of words (falling)
          that already appear in the selection.
          Please try to keep the questions and answers as short as possible while remaining correct.
          Your goal should be that if someone else reads these instructions, the selection, and your question,
          they are likely to write your answer word-for-word.
          If your response has questions that are all identical, do not use words from the selection,
          or are clearly not English, it will be rejected.
          Otherwise, your work will be approved in at most one hour.
          Each HIT should take less than ${numQAs / 2} minutes to complete.
          If at any point you have complaints, questions, or concerns,
          please write them in the "Feedback" field so we may improve the task.
          """),
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
        <FrameHeight>600</FrameHeight>
      </HTMLQuestion>
    """.trim
    Question(question, upickle.write(qData._1))
  }

  override final def extractPrompt(q: Question): OpenFormPrompt = {
    val path = upickle.read[CoNLLSentencePath](q.annotation)
    val sentence = FileManager.getCoNLLSentence(path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)
    (path, sentenceString)
  }

  override final def extractResponse(annotation: Annotation): OpenFormResponse = {
    val answerXML = annotation.answer
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
