package example.tqa

case class TQASentenceId(
  topicId: String,
  sentenceIndex: Int)

object TQASentenceId {
  def toString(sid: TQASentenceId) = sid match {
    case TQASentenceId(topicId, sentenceIndex) =>
      s"${topicId}_${sentenceIndex}"
  }
}
