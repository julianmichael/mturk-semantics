package example.tqa

sealed trait SentenceId

case class TusharSentenceId(index: Int) extends SentenceId

case class TQASentenceId(
  topicId: String,
  sentenceIndex: Int) extends SentenceId

object SentenceId {
  def toString(sid: SentenceId) = sid match {
    case TQASentenceId(topicId, sentenceIndex) =>
      s"${topicId}_${sentenceIndex}"
    case TusharSentenceId(index) =>
      s"TUSHAR_$index"
  }
}
