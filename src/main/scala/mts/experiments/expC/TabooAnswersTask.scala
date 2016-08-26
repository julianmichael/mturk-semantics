package mts.experiments.expC

import mts.core._
import mts.util._
import mts.conll._
import mts.tasks._

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import upickle.default.Writer

object TabooAnswersTask extends TaskSpecification[TabooAnswersPrompt, TabooAnswersResponse] {

  // HIT type fields
  final override val reward = 0.10
  final override val title = s"Write questions and answers about a sentence"
  final override val description = s"""
    Write several questions and answers about a sentence, avoiding the answers shown on a blacklist.
  """.trim
  final override val keywords: String = "language,english,question answering"

  // other fields

  // this is always going to be 1, because we want to pull down the answer to make a new taboo list
  final override val numAssignmentsPerHIT: Int = 1

  // QA specification methods

  final override def createQuestionXML(prompt: TabooAnswersPrompt): String = {
    import scalatags.Text.all._
    val sentence = FileManager.getCoNLLSentence(prompt.path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)
    val tabooListJS = {
      val normStringList = prompt.tabooList
        .map(_.replaceAll("""[\p{Punct} ]""", "").toLowerCase)
        .map(str => s"'$str'")
        .mkString(", ")
      s"[$normStringList]"
    }
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
        p("""This task is for an academic research project by the natural language processing group at the University of Washington.
          We wish to deconstruct the meanings of English sentences into a list of questions and answers.
          You will be presented with a selection of English text, usually a sentence or phrase.
          Your task is to write 2 simple questions, and their answers, satisfying the following criteria:"""),
        ul(
          li("""The question must be an open-ended, grammatical English question about the meaning of the selection,
             and not, for example, the positions of the words."""),
          li("""The question must contain a content word (such as a name, descriptor, or verb other than "be" or "do"),
             or a derivative of such a word, from the selection."""),
          li("""The answer must only contain words (or derivatives of words) that appear in the selection,
             with the exception ONLY of pronouns (such as I, he, it, them) and the words "a", "an", and "the"."""),
        li("""Both the question and answer should use as few specific words from the sentence as possible.
             """),
        li("""The question should be as short as possible while remaining answerable, and
             the answer should be as short as possible while remaining correct.
             """)),
        p("""Consider the sentence: "If the UK is unwilling to accept the free movement of labour,
          it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person."
          Acceptable questions and answers may include:"""),
        ul(
          li("""Who might be unwilling to do something? --- the UK"""),
          li("""How much of a decrease? --- 2.6 per cent"""),
          li("""What would lead to a decrease? --- trade falling""")),
        p("""
          Note that the answers only contain words (the, UK, trade, 2.6, per, cent) or derivatives of words (falling)
          that already appear in the selection. """,
          b("Unacceptable"), """ questions/answers may include:"""),
        ol(
          li("""What might happen? --- trade will fall"""),
          li("""Would income increase or decrease? --- decrease"""),
          li("""Would income decrease by more than 3 per cent? --- no""")),
        p("""Question 1 fails to include a content word from the sentence.
          Question 2 is not open-ended: it provides answer choices. This is not allowed.
          Question 3 is also not open-ended. """,
          b("""Yes/no questions are not allowed, even if the word "yes" or "no" appears in the sentence. """),
          """Also, please phrase your question from the point of view of the speaker,
          i.e., if the word "I" is used in the sentence, please use "I" or "me" in your questions and answers.
          """),
        p("""Finally, there is one more twist: next to the text fields you will see the """, b("Answer Blacklist"), """.
          None of your answers may appear on the blacklist (ignoring case and punctuation),
          or your work will be rejected.
          To prevent accidents and help things go quickly,
          the text field will turn red and the submit button will be disabled
          if your answer appears on the blacklist.
          (Note: other people's answers will appear in the blacklist,
          including invalid answers that we haven't filtered out yet.)
          """),
        p("""Your goal should be that if someone else reads these instructions, the selection, and your question,
          they will write your answer word-for-word.
          If your work satisfies all of the above criteria,
          it will be approved in at most one hour.
          Each HIT should take less than 2 minutes to complete.
          If at any point you have complaints, questions, or concerns,
          please write them in the "Feedback" field so we may improve the task.
          """),
        hr(),
        p("""Please write 2 questions and answers about the following sentence:"""),
        blockquote(sentenceString),
        div(
          float.left,
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
            (0 until 2).map(makeQAElement),
            p(
              input(
                `type` := "text",
                name := s"feedback",
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
              value := "submit"))),
        div(
          float.left,
          answerBlacklist(prompt.tabooList)
        ),
        script(
          `type` := "text/javascript")(
          raw(javaScriptIndexOf + """
            turkSetAssignmentID();
            if($('#assignmentId').attr('value') === 'ASSIGNMENT_ID_NOT_AVAILABLE') {
              $('input').attr('readonly', true);
            }
            var tabooList = """ + tabooListJS + """;
            $('.answerField').on('input', function() {
              var textField = $(this);
              var normalizedAnswer = textField.val()
                .replace(/[!\"#$%&\'()*+,-./:;<=>?@\[\\\]^_`{|}~ ]/g, '')
                .toLowerCase();
              if(tabooList.indexOf(normalizedAnswer) >= 0) {
                textField.css('background-color', '#FFAAAA');
                $('#submitButton').attr('disabled', true);
              } else {
                textField.css('background-color', '#FFFFFF');
                $('#submitButton').attr('disabled', false);
              }
            })
          """))
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
    question
  }

  final override def extractResponse(answerXML: String): TabooAnswersResponse = {
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
    val qaPairs = (0 until answers.size / 2)
      .map(i => (answers.find(a => a._1.equals(s"question-$i")).get._2,
                 answers.find(a => a._1.equals(s"answer-$i")).get._2))
      .toList
    TabooAnswersResponse(qaPairs)
  }

  final override def extractFeedback(answerXML: String): String = {
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
    answers.find(a => a._1.equals("feedback")).get._2
  }

  // private utility methods

  private[this] final val pageFont = "14px Helvetica"
  private[this] final def answerBlacklist(tabooAnswers: List[String]) = {
    import scalatags.Text.all._
    div(
      h3(
        marginLeft := 5,
        marginTop := 0,
        marginBottom := 0,
        "Answer Blacklist"),
      ul(tabooAnswers.map(ans => li(ans)))
    )
  }
  private[this] final def makeQAElement(questionNum: Int) = {
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
        `class` := "answerField",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      )
    )
  }

}
