package example.qpose

import cats.Show
import cats.implicits._

import turksem.gapfill.Answer
import turksem.gapfill.InstantiatedQuestion
import turksem.gapfill.JudgedQuestion
import turksem.gapfill.QuestionGuesserProducer
import turksem.gapfill.QuestionGuesser
import turksem.gapfill.QuestioningState
import turksem.gapfill.QuestionTemplate
import turksem.gapfill.QuestionTemplateAlignment
import turksem.gapfill.SentenceWord
import turksem.gapfill.TriggerSlot
import turksem.gapfill.PreviousAnswerSpan

import turksem.util._

import example.emnlp2017.SentenceId
import example.emnlp2017.SentenceIdHasTokens

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text

case class QGuesser(
  starterTemplateProbsByCoarseGrainedLabel: Map[String, List[(QuestionTemplate[TriggerSlot], Double)]],
  questionExpansionProbabilities: Map[QuestionTemplate[TriggerSlot], NextQuestionProbabilities]) {

  // question guesser producer impl
  def makeClientGuesser(sid: SentenceId)(implicit inflections: Inflections): SentenceQGuesser = SentenceQGuesser(
    getInflectionalSentence(sid),
    starterTemplateProbsByCoarseGrainedLabel,
    questionExpansionProbabilities
  )

  // TODO
  def update(sid: SentenceId, qas: List[(InflectionalWord, JudgedQuestion)]): QGuesser = this
}
object QGuesser {
  def empty: QGuesser = QGuesser(
    Map.empty[String, List[(QuestionTemplate[TriggerSlot], Double)]],
    Map.empty[QuestionTemplate[TriggerSlot], NextQuestionProbabilities]
  )

  implicit val qGuesserQuestionGuesserProducer = new QuestionGuesserProducer[QGuesser] {
    override type ClientGuesser = SentenceQGuesser
    override type SentenceId = example.emnlp2017.SentenceId
    override def makeClientGuesser(a: QGuesser, sid: SentenceId)(implicit inflections: Inflections): SentenceQGuesser = a.makeClientGuesser(sid)
    override def update(a: QGuesser, sid: SentenceId, qas: List[(InflectionalWord, JudgedQuestion)]) = a.update(sid, qas)
    override def empty: QGuesser = QGuesser.empty
  }
}
