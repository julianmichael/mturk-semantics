package example.tqa

import cats.Order
import cats.implicits._

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

  implicit val wikiSentenceIdOrder: Order[WikiSentenceId] = new Order[WikiSentenceId] {
    override def compare(x: WikiSentenceId, y: WikiSentenceId) = (x, y) match {
      case (WikiSentenceId(xPath), WikiSentenceId(yPath)) =>
        val domainCmp = Order[String].compare(xPath.filePath.domain, yPath.filePath.domain)
        if(domainCmp != 0) domainCmp else {
          val suffixCmp = Order[String].compare(xPath.filePath.suffix, yPath.filePath.suffix)
          if(suffixCmp != 0) suffixCmp else {
            val paragraphCmp = Order[Int].compare(xPath.paragraphNum, yPath.paragraphNum)
            if(paragraphCmp != 0) paragraphCmp else {
              Order[Int].compare(xPath.sentenceNum, yPath.sentenceNum)
            }
          }
        }
    }
  }
  implicit val wikiSentenceIdOrdering = wikiSentenceIdOrder.toOrdering

  implicit val tqaSentenceIdOrder: Order[TQASentenceId] = new Order[TQASentenceId] {
    override def compare(x: TQASentenceId, y: TQASentenceId) = (x, y) match {
      case (TQASentenceId(xTopic, xSentenceNum), TQASentenceId(yTopic, ySentenceNum)) =>
        val topicCmp = Order[String].compare(xTopic, yTopic)
        if(topicCmp != 0) topicCmp else Order[Int].compare(xSentenceNum, ySentenceNum)
    }
  }
  implicit val tqaSentenceIdOrdering = tqaSentenceIdOrder.toOrdering

  implicit val sentenceIdOrder: Order[SentenceId] = new Order[SentenceId] {
    def compare(x: SentenceId, y: SentenceId) = (x, y) match {
      case (TQASentenceId(_, _), WikiSentenceId(_)) => -1
      case (WikiSentenceId(_), TQASentenceId(_, _)) => 1
      case (x @ TQASentenceId(_, _), y @ TQASentenceId(_, _)) =>
        Order[TQASentenceId].compare(x, y)
      case (x @ WikiSentenceId(_), y @ WikiSentenceId(_)) =>
        Order[WikiSentenceId].compare(x, y)
    }
  }
  implicit val sentenceIdOrdering = sentenceIdOrder.toOrdering

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
