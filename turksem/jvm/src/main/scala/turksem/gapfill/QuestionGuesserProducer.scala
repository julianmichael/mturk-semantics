package turksem.gapfill

import turksem.util.InflectionalWord

import nlpdata.datasets.wiktionary.Inflections

import simulacrum._
import scala.language.implicitConversions

@typeclass trait QuestionGuesserProducer[A] {
  type ClientGuesser
  type SentenceId

  @op("makeClientGuesser")
  def makeClientGuesser(a: A, sid: SentenceId)(implicit inflections: Inflections): ClientGuesser

  @op("update")
  def update(a: A, sid: SentenceId, qas: List[(InflectionalWord, JudgedQuestion)]): A

  def empty: A
}
