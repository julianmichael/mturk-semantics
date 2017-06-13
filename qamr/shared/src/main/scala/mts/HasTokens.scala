package mts

trait HasTokens[A] {
  def getTokens(a: A): Vector[String]
}
