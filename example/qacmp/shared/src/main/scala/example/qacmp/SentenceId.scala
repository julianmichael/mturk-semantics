package example.qacmp

case class SentenceId(value: String) {
  override def toString = value
}
object SentenceId {
  def toString(sid: SentenceId) = sid.value
  def fromString(str: String) = SentenceId(str)
}
