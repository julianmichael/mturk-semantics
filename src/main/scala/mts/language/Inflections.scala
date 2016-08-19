package mts.language

import mts.util._
import mts.util.LowerCaseStrings._

class Inflections(
  private[this] val inflDict: VerbInflectionDictionary
) {
  import Inflections._

  // has expected array structure
  def getInflectedForms(word: LowerCaseString): Option[List[LowerCaseString]] =
    Option(inflDict.getBestInflections(word)).map(l => l.map(_.lowerCase).toList)

  def getAllForms(word: LowerCaseString): Set[LowerCaseString] = {
    val extras: Set[LowerCaseString] = extraForms.get(getUninflected(word).getOrElse(word)).getOrElse(Set.empty[LowerCaseString])
    def beIfHasWord(wordSet: Set[LowerCaseString]) = wordSet.onlyIf(_.contains(word))
    List(doVerbs, beVerbs, willVerbs, haveVerbs, wouldVerbs, negationWords)
      .map(beIfHasWord).flatten.headOption.map(_.toSet)
      .orElse(getInflectedForms(word).map(_.toSet))
      .getOrElse(Set(word)) ++ extras
  }

  def hasInflectedForms(word: LowerCaseString) = !getInflectedForms(word).isEmpty

  def getUninflected(word: LowerCaseString): Option[LowerCaseString] = {
    if(isCopulaVerb(word)) {
      return Some("be".lowerCase);
    } else {
      Option(inflDict.getBestInflections(word))
        .map(infl => infl(0).lowerCase)
    }
  }

  def isUninflected(word: LowerCaseString): Boolean = getUninflected(word).exists(_ == word)

  def isModal(word: LowerCaseString) = modalVerbs.contains(word)

  def isCopulaVerb(word: LowerCaseString) = beVerbs.contains(word)

  def getNormalizedModal(verb: LowerCaseString) = if(verb.equalsIgnoreCase("ca")) "can" else verb

  // TODO: add tense and stuff. not necessary right now.
  // See VerbHelper from the HITL project in EasySRL for more.
}
object Inflections {
  final val doVerbs = Set(
    "do", "does", "did", "done").map(_.lowerCase)
  final val beVerbs = Set(
    "be", "being", "been",
    "am", "'m",
    "is", "'s", "ai",
    "are", "'re",
    "was", "were").map(_.lowerCase)
  val willVerbs = Set(
    "will", "'ll", "wo").map(_.lowerCase)
  val haveVerbs = Set(
    "have", "having", "'ve", "has", "had", "'d").map(_.lowerCase)
  val wouldVerbs = Set(
    "would", "'d").map(_.lowerCase)
  val modalVerbs = Set(
    "can", "ca",
    "could",
    "may", "might", "must",
    "shall", "should", "ought").map(_.lowerCase) ++ wouldVerbs
  val auxiliaryVerbs =
    doVerbs ++ beVerbs ++ willVerbs ++ haveVerbs ++ modalVerbs
  val negationWords = Set(
    "no", "not", "'nt").map(_.lowerCase)

  // maps an uninflected verb to extra forms of it that aren't in wiktionary
  val extraForms = Map[LowerCaseString, Set[LowerCaseString]](
    "dream".lowerCase -> Set("dreamt").map(_.lowerCase),
    "leap".lowerCase -> Set("leapt").map(_.lowerCase)
  )
}
