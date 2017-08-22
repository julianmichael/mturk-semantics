package turksem.qasrl

import cats.data.NonEmptyList

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

object Slots {
  // slots:
  // WH, AUX, PH1, TRG, PH2, PP, PH3

  val whChoices = NonEmptyList.of(
    "Who", "What", "When", "Where", "Why",
    "How", "How much"
  )

  val auxChoices = NonEmptyList.of(
    "", List(
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
    ).map(" " + _): _*)


  // val beOptions = NonEmptyList.of(
  //   "is", "are", "was", "isn't", "aren't", "wasn't", "weren't"
  // ).map(_.lowerCase)

  // val haveOptions = NonEmptyList.of(
  //   "has", "have", "had",
  //   "hasn't", "haven't", "hadn't"
  // ).map(_.lowerCase)

  // ph1 and ph2
  val subjChoices = NonEmptyList.of(
    "", List(
      "someone", "something"
    ).map(" " + _): _*)

  val objChoices = NonEmptyList.of(
    "", List(
      "someone", "something"
    ).map(" " + _): _*)

  val obj2Choices = NonEmptyList.of(
    "", List(
      "someone", "something", "somewhere",
      "do", "doing", "do something", "doing something"
    ).map(" " + _): _*
  )

  val questionMark = NonEmptyList.of("?")

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

  def getPrepositionSlotChoices(
    tokens: Vector[String]
  ): NonEmptyList[String] = {
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

    NonEmptyList.of("", chosenPrepositions.toSeq.map(" " + _): _*)
  }

  def getVerbSlotChoices(
    forms: InflectedForms
  ): NonEmptyList[String] = {
    import forms._
    NonEmptyList.of(
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
    ).map(" " + _)
  }

}
