package qamr

/** Typeclass for types that can be rendered as a list of tokens.
  * This typeclass restriction holds of whatever you are using to identify
  * "sentences" that can be annotated with QAMR,
  * allowing you to use your own data type as long as it has tokens that can be
  * indexed into/highlighted/etc.
  */
trait HasTokens[-A] {
  /** Returns a vector of Penn Treebank style tokens. */
  def getTokens(a: A): Vector[String]
}
