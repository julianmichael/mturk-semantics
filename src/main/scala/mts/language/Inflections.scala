package mts.language

import mts.util._

class Inflections(
  private[this] val inflDict: VerbInflectionDictionary
) {
  import Inflections._

  // has expected array structure
  def getInflectedForms(word: String): Option[List[String]] =
    Option(inflDict.getBestInflections(word.toLowerCase())).map(_.toList)

  def getAllForms(word: String): List[String] = {
    val w = word.toLowerCase
    val extras = extraForms.get(getUninflected(w).getOrElse(w)).getOrElse(Nil)
    def beIfHasWord(wordSet: Set[String]) = wordSet.onlyIf(_.contains(w))
    List(doVerbs, beVerbs, willVerbs, haveVerbs, wouldVerbs, negationWords)
      .map(beIfHasWord).flatten.headOption.map(_.toList)
      .orElse(Option(inflDict.getBestInflections(w)).map(_.toList))
      .getOrElse(List(word)) ++ extras
  }

  def hasInflectedForms(word: String) = !getInflectedForms(word).isEmpty

  def getUninflected(word: String): Option[String] = {
    if(isCopulaVerb(word)) {
      return Some("be");
    } else {
      Option(inflDict.getBestInflections(word.toLowerCase()))
        .map(infl => infl(0))
    }
  }

  def isUninflected(word: String): Boolean = getUninflected(word).exists(_ == word)

  def isModal(word: String) = modalVerbs.contains(word.toLowerCase)

  def isCopulaVerb(word: String) = beVerbs.contains(word.toLowerCase)

  def getNormalizedModal(verb: String) = if(verb.equalsIgnoreCase("ca")) "can" else verb

  // TODO: add tense and stuff. not necessary right now.
  // See VerbHelper from the HITL project in EasySRL for more.
}
object Inflections {
  final val doVerbs = Set(
    "do", "does", "did", "done")
  final val beVerbs = Set(
    "be", "being", "been",
    "am", "'m",
    "is", "'s", "ai",
    "are", "'re",
    "was", "were")
  val willVerbs = Set(
    "will", "'ll", "wo")
  val haveVerbs = Set(
    "have", "having", "'ve", "has", "had", "'d")
  val wouldVerbs = Set(
    "would", "'d")
  val modalVerbs = Set(
    "can", "ca",
    "could",
    "may", "might", "must",
    "shall", "should", "ought") ++ wouldVerbs
  val auxiliaryVerbs =
    doVerbs ++ beVerbs ++ willVerbs ++ haveVerbs ++ modalVerbs
  val negationWords = Set(
    "no", "not", "'nt")

  // maps an uninflected verb to extra forms of it that aren't in wiktionary
  val extraForms = Map(
    "dream" -> List("dreamt"),
    "leap" -> List("leapt")
  )
}
