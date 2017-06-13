package mts.emnlp2017

import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._

sealed trait SentenceId {
  def readableFileString = this match {
    case PTBSentenceId(path) => s"PTB:${path.filePath.suffix}"
    case WikiSentenceId(path) => s"Wiki1k:${path.filePath.get}"
  }
  def readableSentenceIndex = this match {
    case PTBSentenceId(path) => s"${path.sentenceNum}"
    case WikiSentenceId(path) => s"${path.paragraphNum}:${path.sentenceNum}"
  }

  def getPTB = Some(this) collect { case id @ PTBSentenceId(_) => id }
  def isPTB = getPTB.nonEmpty

  def getWiki = Some(this) collect { case id @ WikiSentenceId(path) => id }
  def isWiki = getWiki.nonEmpty
}
case class PTBSentenceId(path: PTBSentencePath) extends SentenceId
case class WikiSentenceId(path: Wiki1kSentencePath) extends SentenceId
