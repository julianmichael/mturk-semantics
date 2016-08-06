package mts.qa

import mts.CoNLLSentencePath
import mts.FileManager
import mts.Question
import mts.Annotation
import mts.TextRendering

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

case class OpenFormMultipleQASpec(numQAs: Int = 3) extends QASpec {
  type QuestionData = (CoNLLSentencePath, String) // source of sentence, and sentence to annotate
  type AnswerData = List[(String, String)] // QA pairs

  def makeQAField(caption: String, id: String) =
    s"""
    <Question>
      <QuestionIdentifier>$id</QuestionIdentifier>
      <IsRequired>true</IsRequired>
      <QuestionContent>
        <Text>$caption</Text>
      </QuestionContent>
      <AnswerSpecification>
        <FreeTextAnswer>
          <NumberOfLinesSuggestion>1</NumberOfLinesSuggestion>
        </FreeTextAnswer>
      </AnswerSpecification>
    </Question>
    """.trim

  final def makeQuestion(id: Int) =
    makeQAField(s"Question $id", s"question-$id") + makeQAField(s"Answer $id", s"answer-$id")

  final def createQuestion(qData: QuestionData): Question =
    Question(s"""
    <?xml version="1.0" encoding="UTF-8"?>
    <QuestionForm xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionForm.xsd">
      <Overview>
        <Text>This task is for an academic research project by the natural language processing group at the University of Washington. We wish to deconstruct the meanings of English sentences into a list of questions and answers. You will be presented with an English sentence. Your task is to write three simple questions, and their answers, satisfying the following criteria:</Text>
        <List>
          <ListItem>The question must be about the meaning of the sentence, and not, for example, the positions of the words.</ListItem>
          <ListItem>The question must contain a content word (such as a name, descriptor, or verb other than "be" or "do"), or a derivative of such a word, from the sentence.</ListItem>
          <ListItem>The answer must only contain words (or derivatives of words) that appear in the sentence.</ListItem>
        </List>
        <Text> Consider the sentence: "If the UK is unwilling to accept the free movement of labour, it is likely trade will fall by more, leading to a 2.6 per cent decrease in income per person." Acceptable questions and answers may include:</Text>
        <List>
          <ListItem>Who might be unwilling to do something? --- the UK</ListItem>
          <ListItem>What would lead to a decrease in income? --- trade falling</ListItem>
          <ListItem>How much of a decrease? --- 2.6 per cent</ListItem>
        </List>
        <Text>Please try to keep the questions and answers as short as possible while remaining correct. Your goal should be that if someone else reads these instructions, the sentence, and your question, they are likely to write your answer word-for-word. If your response has questions that are all identical, do not use words from the sentence, or are clearly not English, it will be rejected. Otherwise, your work will be approved in at most one hour. Each HIT should take less than two minutes to complete.</Text>
      </Overview>
      <Overview>
        <Text>Please write three questions and answers about the following sentence:</Text>
        <Text>${qData._2}</Text>
      </Overview>
      ${(1 to numQAs).map(makeQuestion).mkString("\n")}
    </QuestionForm>
    """.trim, upickle.write(qData._1))

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
    qaPairs
  }
}
