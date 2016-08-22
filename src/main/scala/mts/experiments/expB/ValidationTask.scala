package mts.experiments.expB

import mts.core._
import mts.util._
import mts.conll._
import mts.tasks._

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import scala.util.{Try, Success, Failure}

case class ValidationTask(
  final override val numAssignmentsPerHIT: Int = 5
) extends MTurkTask[ValidationPrompt, ValidationResponse] {

  final override val reward = 0.10
  final override val title = s"Answer questions about a sentence"
  final override val description = s"""
    Evaluate and answer several questions a sentence's meaning.
  """.trim
  final override val keywords: String = "language,english,question answering"

  final val pageFont = "14px Helvetica"

  final def makeQAElement(questionNum: Int, vQuestion: ValidationQuestion) = {
    import scalatags.Text.all._
    p(
      margin := 0,
      padding := 0
    )(
      label(
        vQuestion.question,
        `class` := "questionLabel",
        `for` := s"answer-$questionNum",
        float.left,
        margin := 1,
        padding := 1,
        font := pageFont
      ),
      input(
        id := s"answer-$questionNum",
        `class` := s"answerField",
        required,
        `type` := "text",
        name := s"answer-$questionNum",
        placeholder := "Answer",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      ),
      input(
        id := s"invalid-$questionNum",
        `type` := "checkbox",
        `class` := "invalidQuestionCheckbox",
        name := s"invalid-$questionNum",
        margin := 1,
        padding := 1
      ),
      span(
        "Invalid question",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      )
    )
  }

  final def createQuestion(qData: ValidationPrompt): Question = {
    val ValidationPrompt(path, vQuestions) = qData
    val sentence = FileManager.getCoNLLSentence(path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)

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
          You will be presented with a selection of English text, usually a sentence or phrase,
          and a list of questions that were written by other annotators.
          Your task is to ensure that the questions are valid, and, if they are valid, to write valid answers for them.
          Valid questions and answers satisfy the following criteria:
        """),
        ul(
          li("""The question must be about the meaning of the selection, and not, for example, the positions of the words."""),
          li("""The question must contain a content word (such as a name, descriptor, or verb other than "be" or "do"),
             or a derivative of such a word, from the selection."""),
          li("""The answer must only contain words (or derivatives of words) that appear in the selection.""")),
        p("""Consider the sentence: "If the UK is unwilling to accept the free movement of labour,
          it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person."
          Valid questions and answers may include:"""),
        ul(
          li("""Who might be unwilling to do something? --- the UK"""),
          li("""What would lead to a decrease in income? --- trade falling"""),
          li("""How much of a decrease? --- 2.6 per cent""")),
        p(s"""
          Note that the answers only contain words (the, UK, trade, 2.6, per, cent) or derivatives of words (falling)
          that already appear in the selection.
          While the question does not have to be a whole sentence (see, for example, "How much of a decrease?"),
          it must be grammatical, fluent English and validly answerable from the sentence.
          If the question is invalid for any reason, please check the box next it labeled "Invalid question."
          Otherwise, write your (valid) answer to the question in the provided text field.
          Please make the answer as short as possible while remaining specific enough to be correct.
          Your goal should be for your answer to agree word-for-word with what other people working on this task will write.
          """),
        p(s"""
          If your response has answers that are all identical, do not use words from the selection,
          or are clearly not English, it will be rejected.
          If your validity judgments consistently disagree with other annotators, you may be blocked.
          Otherwise, your work will be approved in at most one hour.
          Each HIT should take less than 1 minute to complete.
          If at any point you have complaints, questions, or concerns,
          please write them in the "Feedback" field so we may improve the task.
          """),
        hr(),
        p(s"""Please evaluate/answer the questions about the following sentence:"""),
        blockquote(sentenceString),
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
          vQuestions.zipWithIndex.map { case (q, i) => makeQAElement(i, q) },
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
              $('input').attr('disabled', true)
            } else {
              $('.invalidQuestionCheckbox').change(function() {
                var textField = $(this).siblings('.answerField')
                if(this.checked) {
                  textField.attr('readonly', true)
                  textField.css('background-color', '#DDDDDD')
                  textField.css('color', '#AAAAAA')
                  textField.data('value', textField.val())
                  textField.val('N/A')
                } else {
                  textField.attr('readonly', false)
                  textField.css('background-color', '#FFFFFF')
                  textField.css('color', '#000000')
                  textField.val(textField.data('value'))
                }
              })
            }

            var questionWidths = $('.questionLabel').map(function() {
              return $(this).width() || -Infinity;
            }).toArray();
            $('.questionLabel').width(Math.max.apply(Math, questionWidths));

          """)
      )
    )
    val questionXML = s"""
      <?xml version="1.0" encoding="UTF-8"?>
      <HTMLQuestion xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd">
        <HTMLContent><![CDATA[
          <!DOCTYPE html>${page.render}
        ]]></HTMLContent>
        <FrameHeight>600</FrameHeight>
      </HTMLQuestion>
    """.trim
    Question(questionXML, upickle.write(qData))
  }

  final def extractPrompt(q: Question): ValidationPrompt = {
    upickle.read[ValidationPrompt](q.annotation)
  }

  final def extractResponse(annotation: Annotation): ValidationResponse = {
    val answerXML = annotation.answer
    val workerId = annotation.workerId
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
    val numAnswers = answers.filter(_.getQuestionIdentifier.startsWith("answer")).size
    val validationAnswers = (0 until numAnswers)
      .map(i => answers.find(a => a.getQuestionIdentifier.equals(s"answer-$i")).get.getFreeText)
      .map(ansString => if (ansString.equals("N/A")) InvalidQuestion(workerId) else Answer(ansString, workerId))
      .toList
    val comment = answers.find(a => a.getQuestionIdentifier.equals("comments")).get.getFreeText
    ValidationResponse(validationAnswers, comment)
  }
}
