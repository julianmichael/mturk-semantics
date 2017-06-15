package qamr

case class IsStopword(predicate: String => Boolean) extends (String => Boolean) {
  override def apply(token: String): Boolean = predicate(token)
}
