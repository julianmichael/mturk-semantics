package example.emnlp2017

import turksem._
import example.emnlp2017._
import turksem.qamr._
import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.PosTags
// import nlpdata.datasets.wiktionary.Inflections

package object analysis {

  case class ContiguousSpan(begin: Int, end: Int) {
    def indices = (begin to end).toSet
    def contains(i: Int): Boolean = begin <= i || end >= i
    def getTokens(id: SentenceId): Vector[String] =
      id.tokens.slice(begin, end)
  }

}
