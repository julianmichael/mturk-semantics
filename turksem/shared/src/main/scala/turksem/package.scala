import nlpdata.util.LowerCaseStrings._

package object turksem {

  /** We require questions to begin with one of these words. */
  val whWords = Set("who", "what", "when", "where", "why", "how", "which", "whose").map(_.lowerCase)

  def beginsWithWh(s: String): Boolean = whWords.exists(w => s.toLowerCase.startsWith(w))

}
