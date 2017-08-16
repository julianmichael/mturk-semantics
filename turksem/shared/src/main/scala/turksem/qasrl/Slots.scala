package turksem.qasrl

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

case class SlotChoices(
  tokens: Set[String],
  canSkip: Boolean) {
  val lowerCaseTokens = tokens.map(_.lowerCase)
  def admits(token: LowerCaseString): Boolean = lowerCaseTokens.contains(token)
  def prefixMatches(token: LowerCaseString): Set[String] =
    tokens.filter(_.toLowerCase.startsWith(token))
  def admitsPrefix(token: LowerCaseString): Boolean =
    prefixMatches(token).nonEmpty
}

object Slots {
  // slots:
  // WH, AUX, PH1, TRG, PH2, PP, PH3

  val whChoices = SlotChoices(
    tokens = Set(
      "Who", "What", "When", "Where", "Why",
      "How", "How much"
    ),
    canSkip = false)

  val auxChoices = SlotChoices(
    tokens = Set(
      "is", "are", "was", "were",
      "do", "does", "did",
      "has", "have", "had",
      "can", "could", "may",
      "would", "should",
      "might", "must",
      "will",
      "isn't", "aren't", "wasn't", "weren't",
      "don't", "doesn't", "didn't",
      "hasn't", "haven't", "hadn't",
      "can't", "couldn't", "may not",
      "would not", "should not",
      "might not", "must not", "mustn't",
      "won't"
    ),
    canSkip = true)


  // val beOptions = Set(
  //   "is", "are", "was", "isn't", "aren't", "wasn't", "weren't"
  // ).map(_.lowerCase)

  // val haveOptions = Set(
  //   "has", "have", "had",
  //   "hasn't", "haven't", "hadn't"
  // ).map(_.lowerCase)

  // ph1 and ph2
  val subjChoices = SlotChoices(
    tokens = Set("", "someone", "something"),
    canSkip = true)

  val objChoices = SlotChoices(
    tokens = Set("", "someone", "something"),
    canSkip = true)

  val obj2Choices = SlotChoices(
    tokens = Set(
      "", "someone", "something", "somewhere",
      "do", "doing", "do something", "doing something"),
    canSkip = true)

  val mostCommonPrepositions = Set(
    "by", "to", "for", "with", "about",
    "from" // added my own on this line
  ).map(_.lowerCase)

  val lotsOfPrepositions = Set(
		"aboard", "about", "above", "across", "afore", "after", "against", "ahead", "along", "alongside", "amid",
		"amidst", "among", "amongst", "around", "as", "aside", "astride", "at", "atop", "before",
		"behind", "below", "beneath", "beside", "besides", "between", "beyond", "by", "despite", "down",
		"during", "except", "for", "from", "given", "in", "inside", "into", "near", "next",
		"of", "off", "on", "onto", "opposite", "out", "outside", "over", "pace", "per",
		"round", "since", "than", "through", "throughout", "till", "times", "to", "toward", "towards",
		"under", "underneath", "until", "unto", "up", "upon", "versus", "via", "with ", "within",
		"without"
  ).map(_.lowerCase)

  val questionMark = SlotChoices(
    tokens = Set("?"),
    canSkip = false)


  def getPrepositionSlotChoices(
    tokens: Vector[String]
  ): SlotChoices = {
    val lowerTokens = tokens.map(_.lowerCase)
    def isPreposition(lcs: LowerCaseString): Boolean = Slots.lotsOfPrepositions.contains(lcs)
    val newPrepositions = lowerTokens.filter(isPreposition)
    val prepositionBigrams = lowerTokens.sliding(2)
      .filter(_.forall(isPreposition))
      .map(_.mkString(" ").lowerCase)

    val chosenPrepositions: Set[LowerCaseString] = (
      newPrepositions.iterator ++
        prepositionBigrams ++
        Slots.mostCommonPrepositions.iterator).toSet

    SlotChoices(
      tokens = chosenPrepositions.map(_.toString),
      canSkip = true)
  }

  def getVerbSlotChoices(
    forms: InflectedForms
  ): SlotChoices = {
    import forms._
    SlotChoices(
      tokens = Set(
        stem.toString,
        present.toString,
        presentParticiple.toString,
        s"be $presentParticiple",
        s"been $presentParticiple",
        s"have been $presentParticiple",
        past.toString,
        s"being $pastParticiple",
        s"be $pastParticiple",
        s"been $pastParticiple",
        s"have $pastParticiple",
        s"have been $pastParticiple"
      ),
      canSkip = false)
  }

}
