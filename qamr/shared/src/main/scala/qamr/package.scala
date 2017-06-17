import nlpdata.util.LowerCaseStrings._

package object qamr {

  implicit class HasTokensOps[A](a: A)(implicit ht: HasTokens[A]) {
    def tokens: Vector[String] = ht.getTokens(a)
  }

  val whWords = Set("who", "what", "when", "where", "why", "how", "which", "whose").map(_.lowerCase)

  def beginsWithWh(s: String): Boolean = whWords.exists(w => s.toLowerCase.startsWith(w))
}
