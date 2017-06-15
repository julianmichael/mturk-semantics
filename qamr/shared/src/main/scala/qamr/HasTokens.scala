package qamr

trait HasTokens[-A] {
  def getTokens(a: A): Vector[String]
}
