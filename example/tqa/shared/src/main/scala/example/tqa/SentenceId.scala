package example.tqa

import nlpdata.datasets.wiki1k.Wiki1kPath
import nlpdata.datasets.wiki1k.Wiki1kSentencePath

sealed trait SentenceId {
  def getWiki = Some(this) collect { case id: WikiSentenceId => id }
  def isWiki = getWiki.nonEmpty

  def getTQA = Some(this) collect { case id: TQASentenceId => id }
  def isTQA = getTQA.nonEmpty
}

case class TQASentenceId(topicId: String, sentenceNum: Int) extends SentenceId
case class WikiSentenceId(path: Wiki1kSentencePath) extends SentenceId

object SentenceId {

  private[this] val Wiki1kMatch = "Wiki1k:([^:]+):([^:]+):([0-9]+):([0-9]+)".r
  private[this] val TQAMatch = "TQA:([^:]+)_([0-9]+)".r

  // not necessarily used for serialization over the wire, but
  // used for storing to / reading from  the dataset file.
  def toString(sid: SentenceId) = sid match {
    case TQASentenceId(topicId, sentenceIndex) =>
      s"TQA:${topicId}_${sentenceIndex}"
    case WikiSentenceId(path) =>
      s"Wiki1k:${path.filePath.domain}:${path.filePath.suffix}:${path.paragraphNum}:${path.sentenceNum}"
  }

  def fromString(s: String): SentenceId = s match {
    case TQAMatch(topicId, sentenceNum) =>
      TQASentenceId(topicId, sentenceNum.toInt)
    case Wiki1kMatch(domain, suffix, paragraphNum, sentenceNum) =>
      WikiSentenceId(Wiki1kSentencePath(Wiki1kPath(domain, suffix), paragraphNum.toInt, sentenceNum.toInt))
  }

}
