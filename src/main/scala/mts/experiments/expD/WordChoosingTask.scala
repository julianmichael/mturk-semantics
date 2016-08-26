package mts.experiments.expD

import mts.core._
import mts.util._
import mts.conll._
import mts.tasks._

import LowerCaseStrings._

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import upickle.default.Writer

import org.apache.commons.lang.StringEscapeUtils

final class WordChoosingTask(
  final override val numAssignmentsPerHIT: Int
) extends TaskSpecification[WordChoosingPrompt, WordChoosingResponse] {

  import mts.tasks.Config

  // HIT type fields
  final override val reward = 0.15
  final override val title = s"Write questions and answers about a sentence"
  final override val description = s"""
    Choose pairs of words from a sentence, and write questions and answers about them.
    ${bonusCents}c bonus if you choose a rare word pair.
  """.trim
  final override val keywords: String = "language,english,question answering"

  final val bonusCents = 5 // determines HIT type because it appears in the description

  // other fields

  final val numQAs = 3

  // task specialization methods

  final val bonus = bonusCents.toDouble / 100.0

  final override def evaluateAssignment(a: Assignment[WordChoosingResponse]): AssignmentEvaluation = {
    val hit = FileManager.getHIT[WordChoosingPrompt](hitType, a.hitId).toOptionPrinting.get
    val sentence = FileManager.getCoNLLSentence(hit.prompt.path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)
    val allowableWords = getAllAllowableWords(sentence, sentenceString)
    val invalidWords = for {
      WordChoosingResponseItem(qWord, aWord, _, _) <- a.response.items
      invalidWord <- List(qWord.lowerCase, aWord.lowerCase).filterNot(allowableWords.contains)
    } yield invalidWord
    if(invalidWords.isEmpty) {
      Approval("")
    } else {
      Rejection(s"Sentence: $sentenceString\nInvalid question/answer words: ${invalidWords.mkString(", ")}\n" +
                  "Use a modern browser (e.g., Google Chrome) and make sure JavaScript is enabled " +
                  "in order to protect against invalid submissions on the HIT page.")
    }
  }

  // QA specification methods

  private[this] def getAllAllowableWords(sentence: CoNLLSentence, sentenceString: String): Set[LowerCaseString] = {
    (sentenceString.split("\\s+").map(_.lowerCase) ++
       sentence.words.map(_.token).map(_.lowerCase)).toSet
  }

  final override def createQuestionXML(prompt: WordChoosingPrompt): String = {
    val sentence = FileManager.getCoNLLSentence(prompt.path).toOptionPrinting.get
    val sentenceString = TextRendering.renderSentence(sentence)
    val wordsAndTokens = getAllAllowableWords(sentence, sentenceString)
    val jsTokens = wordsAndTokens.toList.map(t => s"'${StringEscapeUtils.escapeJavaScript(t)}'").mkString(", ")
    val jsTokenList = s"[$jsTokens]"
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
        h2("""Task Summary"""),
        p("""This task is for an academic research project by the natural language processing group at the University of Washington.
          We wish to deconstruct the meanings of English sentences into a list of questions and answers.
          You will be presented with a selection of English text, usually a sentence or phrase.
          Your task is:"""),
        ol(
          li("""Choose two words from the selection: a """, b("""question word"""), """ and an """,
             b("""answer word"""), """."""),
          li("""Write a """, b("""question"""), """ about the sentence that contains the question word and its """,
             b("""answer"""), """ which contains the answer word.""")),
        h2("""Requirements"""),
        p("""This task is best fit for native speakers of English.
          Your response must satisfy the following criteria:"""),
        ul(
          li("""The question and answer words are both content words from the selection (not words like """,
             i("""the, a, in, to, of"""), """)."""),
          li("""The question and answer are fluent English and contain the question word and answer word,
             and they are obviously answerable from the sentence."""),
          li("""The question and answer use as few words from the selection as possible."""),
          li("""The question and answer are as short as possible while remaining correct.""")),
        p("""If someone else reads these instructions, the selection, and your question,
          they should be able to write your answer word-for-word.
          You will be rewarded if you choose a question/answer word pair
          that was not picked by any other annotator (see Conditions and Payment below).
          """),
        p("""To aid you, the """, i("""Question word"""), """ and """, i("""Answer word"""),
          """ text fields will turn green when you've written an acceptable word
          (it must match a word in the sentence exactly),
          and red when the word is not in the selection.
          The """, i("""Submit"""), """ button will also be disabled when your choices of question and answer word are not allowed.
          However, the text field will still be green for non-content words like """, i("the"), """ which you should not use.
          """),
        h2("""Example"""),
        p("""Consider the sentence: "If the UK is unwilling to accept the free movement of labour,
          it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person."
          Acceptable questions and answers may include:"""),
        ul(
          li("""decrease / 2.6 / How much of a decrease? / 2.6 per cent"""),
          li("""UK / accept / What might the UK not do? / accept something"""),
          li("""lead / fall / What would lead to something? / trade falling""")),
        p("""
          Note that the answers center around the answer word, changing its form (e.g., from """,
          i("""fall"""), """ to """, i("""falling"""), """) if necessary.
          The answers may contain other words from the sentence in order to make sense, as with """,
          i("""per, cent, trade"""), """. """,
          """However, prefer to instead use a generic word like """, i("""something"""), """ or """, i("""someone"""),
          """ if possible, as with """, i("""accept something"""), """."""),
        h2("""Conditions and Payment"""),
        p("""If your work satisfies the above criteria,
          it will be approved in at most one hour.
          If you choose a question/answer word not in the sentence (verified for you by the text fields turning
          green if they are valid and red if they are not), """, b(""" your response will be rejected. """),
          """If your responses repeatedly fail to meet the above criteria, you will be blocked from this task and future tasks.
          Each HIT should take about one minute to complete.
          For each selection, """,
          b(s"""If at least one of your question / answer word pairs was not chosen by ${numAssignmentsPerHIT - 1} other workers,
          you will be paid a ${bonusCents}c bonus. """),
          """The bonus will be paid out to you as soon as all of the annotations have been gathered for a sentence.
          If at any point you have complaints, questions, or concerns,
          please write them in the "Feedback" field so we may improve the task.
          """),
        hr(),
        p(s"""Please write $numQAs questions and answers about the following selection:"""),
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
            (0 until numQAs).map(makeQAElement),
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
        script(
          `type` := "text/javascript")(
          raw(javaScriptIndexOf + """
            turkSetAssignmentID();
            if($('#assignmentId').attr('value') === 'ASSIGNMENT_ID_NOT_AVAILABLE') {
              $('input').attr('readonly', true);
            }
            // change to Q/A word fields
            var tokens = """ + jsTokenList + """;
            $('.""" + sentenceWordFieldClass + """').on('input', function() {
              var textField = $(this);
              var word = textField.val().toLowerCase();
              if(!word) {
                textField.css('background-color', '#FFFFFF');
              } else if(tokens.indexOf(word) >= 0) {
                $('#submitButton').attr('disabled', false);
                textField.css('background-color', '#88FF88');
              } else {
                textField.css('background-color', '#FFFFFF');
                $('#submitButton').attr('disabled', true);
              }
            });
            $('.""" + sentenceWordFieldClass + """').change(function() {
              var textField = $(this);
              var word = textField.val().toLowerCase();
              if(!word) {
                textField.css('background-color', '#FFFFFF');
              } else if(tokens.indexOf(word) >= 0) {
                textField.css('background-color', '#88FF88');
              } else {
                textField.css('background-color', '#FF8888');
              }
            });
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

  final override def extractResponse(answerXML: String): WordChoosingResponse = {
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
      .toMap
    val items = (0 until numQAs)
      .map(i => WordChoosingResponseItem(
             answers(questionWordIdentifier(i)),
             answers(answerWordIdentifier(i)),
             answers(questionIdentifier(i)),
             answers(answerIdentifier(i))))
      .toList
    WordChoosingResponse(items)
  }

  final override def extractFeedback(answerXML: String): String = {
    import scala.collection.JavaConverters._
    val answers = RequesterService.parseAnswers(answerXML).getAnswer
      .asScala.toList.asInstanceOf[List[QuestionFormAnswersType.AnswerType]]
      .map(ans => (ans.getQuestionIdentifier, ans.getFreeText))
      .toMap
    answers("feedback")
  }

  // private utility methods

  private[this] final val pageFont = "14px Helvetica"
  private[this] final val sentenceWordFieldClass = "sentenceWordField"
  private[this] final def questionWordIdentifier(i: Int) = s"question-word-$i"
  private[this] final def answerWordIdentifier(i: Int) = s"answer-word-$i"
  private[this] final def questionIdentifier(i: Int) = s"question-$i"
  private[this] final def answerIdentifier(i: Int) = s"answer-$i"

  private[this] final def makeQAElement(questionNum: Int) = {
    import scalatags.Text.all._
    p(
      margin := 0,
      padding := 0
    )(
      input(
        `type` := "text",
        required,
        name := questionWordIdentifier(questionNum),
        placeholder := "Question word",
        `class` := sentenceWordFieldClass,
        margin := 1,
        padding := 1,
        width := 120,
        font := pageFont
      ),
      input(
        `type` := "text",
        required,
        name := answerWordIdentifier(questionNum),
        placeholder := "Answer word",
        `class` := sentenceWordFieldClass,
        margin := 1,
        padding := 1,
        width := 120,
        font := pageFont
      ),
      input(
        `type` := "text",
        required,
        name := questionIdentifier(questionNum),
        placeholder := "Question",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      ),
      input(
        `type` := "text",
        required,
        name := answerIdentifier(questionNum),
        placeholder := "Answer",
        margin := 1,
        padding := 1,
        width := 240,
        font := pageFont
      )
    )
  }

}
