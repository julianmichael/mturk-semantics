package turksem.qasrl

import cats.data.NonEmptyList

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

object Slots {
  sealed trait TemplateState
  case object TemplateComplete extends TemplateState
  case class TemplateProgress(
    transitions: NonEmptyList[TemplateTransition]
  ) extends TemplateState

  type TemplateTransition = (String, TemplateState)

  // neither of these should contain "to", which is handled specially

  val mostCommonPrepositions = Set(
    "by", "for", "with", "about",
    "in", "from", "to" // added my own on this line
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

  // convenience functions for constructing states

  def reqState(first: TemplateTransition, rest: TemplateTransition*) = TemplateProgress(
    NonEmptyList.of(first, rest: _*)
  )

  def optStateWithoutSpace(target: TemplateState, all: String*) = TemplateProgress(
    NonEmptyList.of("", all: _*).map(_ -> target)
  )

  def optStateWithSpace(target: TemplateState, all: String*) = TemplateProgress(
    NonEmptyList.of("", all.map(" " + _): _*).map(_ -> target)
  )

  def reqStateWithSpace(target: TemplateState, first: String, rest: String*) = TemplateProgress(
    NonEmptyList.of(first, rest: _*).map(" " + _).map(_ -> target)
  )

  def reqStateWithoutSpace(target: TemplateState, first: String, rest: String*) = TemplateProgress(
    NonEmptyList.of(first, rest: _*).map(_ -> target)
  )
}

class Slots(
  tokens: Vector[String],
  verbInflectedForms: InflectedForms) {

  // process prepositions
  val lowerTokens = tokens.map(_.lowerCase)
  def isPreposition(lcs: LowerCaseString): Boolean = Slots.lotsOfPrepositions.contains(lcs)
  val newPrepositions = lowerTokens.filter(isPreposition)
  val prepositionBigrams = lowerTokens.sliding(2)
    .filter(_.forall(isPreposition))
    .map(_.mkString(" ").lowerCase)

  val allChosenPrepositions: Set[LowerCaseString] = (
    newPrepositions.iterator ++
      prepositionBigrams ++
      Slots.mostCommonPrepositions.iterator
  ).toSet

  val nonToEndingPrepositions = allChosenPrepositions.filterNot(_.endsWith("to"))

  val toEndingPrepositions = allChosenPrepositions.filter(_.endsWith("to"))

  import verbInflectedForms._

  import Slots._

  val qMark = reqState("?" -> TemplateComplete)

  val noPrepObj = optStateWithSpace(
    qMark,
    "someone", "something", "somewhere"
  )

  val prepObj = optStateWithSpace(
    qMark,
    "someone", "something",
    "doing", "doing something"
  )

  val postToObj = optStateWithSpace(
    qMark,
    "someone", "something",
    "do", "do something"
    // "be doing", "be doing something",
    // "have done", "have done something",
    // "have been doing", "have been doing something"
  )
  // the last ones seem like overkill

  val nonToPrep = optStateWithSpace(
    prepObj,
    nonToEndingPrepositions.map(_.toString).toSeq: _*
  )

  val to = optStateWithSpace(
    postToObj,
    toEndingPrepositions.map(_.toString).toSeq: _*
  )

  val prep = TemplateProgress(
    NonEmptyList.of(
      "" -> to,
      "" -> nonToPrep,
      "" -> noPrepObj
    )
  )

  val obj = optStateWithSpace(prep, "someone", "something")

  // follows a have-aux
  val pastParticipleVerb = reqStateWithSpace(
    obj,
    s"been $presentParticiple",
    s"been $pastParticiple",
    pastParticiple.toString)

  // follows a modal
  val infinitiveVerb = reqStateWithSpace(
    obj,
    stem.toString,
    s"be $presentParticiple",
    s"have been $presentParticiple",
    s"be $pastParticiple",
    s"have $pastParticiple",
    s"have been $pastParticiple"
  )

  // follows a do-verb
  val stemVerb = reqStateWithSpace(
    obj,
    stem.toString)

  // follows a be-aux
  val presentParticipleOrPassiveVerb = reqStateWithSpace(
    obj,
    presentParticiple.toString,
    s"being $pastParticiple",
    pastParticiple.toString
  )

  // follows no aux
  val tensedVerb = reqStateWithSpace(
    obj,
    present.toString,
    past.toString
  )

  // neg/subj states carry the verb form through; so, all need to be constructed at construction time

  def postSubjectNegation(targetVerbState: TemplateState) = optStateWithSpace(
    targetVerbState,
    "not")

  def subj(targetVerbState: TemplateState) = reqStateWithSpace(
    postSubjectNegation(targetVerbState),
    "someone", "something", "it"
  )

  def optionalSubj(targetVerbState: TemplateState) = TemplateProgress(
    NonEmptyList.of(
      "" -> subj(targetVerbState),
      "" -> targetVerbState // can skip directly to verb
    )
  )

  def negContraction(subjRequired: Boolean, targetVerbState: TemplateState) = {
    val target = if(subjRequired) subj(targetVerbState) else optionalSubj(targetVerbState)
    optStateWithoutSpace(target, "n't")
  }

  def haveAux(subjRequired: Boolean) = reqStateWithSpace(
    negContraction(subjRequired, pastParticipleVerb),
    "has", "have", "had"
  )

  def infNegContraction(subjRequired: Boolean) = negContraction(subjRequired, infinitiveVerb)

  def modalAux(subjRequired: Boolean) = {
    val infSubj = if(subjRequired) subj(infinitiveVerb) else optionalSubj(infinitiveVerb)
    val infNegContraction = negContraction(subjRequired, infinitiveVerb)
    TemplateProgress(
      NonEmptyList.of(
        " won't" -> infSubj,
        " will" -> infSubj,
        " would" -> infNegContraction,
        " might" -> infNegContraction
      )
    )
  }

  def doAux(subjRequired: Boolean) = reqStateWithSpace(
    negContraction(subjRequired, stemVerb),
    "do", "does", "did"
  )

  def beAux(subjRequired: Boolean) = reqStateWithSpace(
    negContraction(subjRequired, presentParticipleOrPassiveVerb),
    "is", "are", "was", "were"
  )

  def preAux(subjRequired: Boolean) = {
    val tail = NonEmptyList.of(
      "" -> beAux(subjRequired),
      "" -> modalAux(subjRequired),
      "" -> doAux(subjRequired),
      "" -> haveAux(subjRequired))
    val transitions = if(subjRequired) tail else ("" -> tensedVerb) :: tail
    TemplateProgress(transitions)
  }

  val wh = {
    val aux = preAux(subjRequired = false)
    val auxRequiringSubject = preAux(subjRequired = true)
    TemplateProgress(
      NonEmptyList.of(
        "Who" -> aux,
        "What" -> aux,
        "When" -> auxRequiringSubject,
        "Where" -> auxRequiringSubject,
        "Why" -> auxRequiringSubject,
        "How" -> auxRequiringSubject,
        "How much" -> auxRequiringSubject
      )
    )
  }

  def start = wh



  // val templateChoices = Vector[NonEmptyList[String]](
  //   Slots.whChoices,
  //   Slots.auxChoices,
  //   Slots.negContractionChoices,
  //   Slots.subjChoices,
  //   Slots.negAfterSubjectChoices,
  //   verbChoices,
  //   Slots.objChoices,
  //   prepositionChoices,
  //   Slots.obj2Choices,
  //   Slots.questionMark)



  // val whChoices = NonEmptyList.of(
  //   "Who", "What", "When", "Where", "Why",
  //   "How", "How much"
  // )

  // val auxChoices = NonEmptyList.of(
  //   "", List(
  //     "is", "are", "was", "were",
  //     "do", "does", "did",
  //     "has", "have", "had",
  //     "might", "would", "should",
  //     "will", "won't" // exception for "won't" so we don't get a weird "wo" in the autocomplete
  //   ).map(" " + _): _*)

  // val negContractionChoices = NonEmptyList.of("", "n't")

  // val negAfterSubjectChoices = NonEmptyList.of("", " not")

  // // val beOptions = NonEmptyList.of(
  // //   "is", "are", "was", "isn't", "aren't", "wasn't", "weren't"
  // // ).map(_.lowerCase)

  // // val haveOptions = NonEmptyList.of(
  // //   "has", "have", "had",
  // //   "hasn't", "haven't", "hadn't"
  // // ).map(_.lowerCase)

  // // ph1 and ph2
  // val subjChoices = NonEmptyList.of(
  //   "", List(
  //     "someone", "something"
  //   ).map(" " + _): _*)

  // val objChoices = NonEmptyList.of(
  //   "", List(
  //     "someone", "something"
  //   ).map(" " + _): _*)

  // val obj2Choices = NonEmptyList.of(
  //   "", List(
  //     "someone", "something", "somewhere",
  //     "do", "doing", "do something", "doing something"
  //   ).map(" " + _): _*
  // )

  // val questionMark = NonEmptyList.of("?")

  // def getPrepositionSlotChoices(
  //   tokens: Vector[String]
  // ): NonEmptyList[String] = {
  //   val lowerTokens = tokens.map(_.lowerCase)
  //   def isPreposition(lcs: LowerCaseString): Boolean = Slots.lotsOfPrepositions.contains(lcs)
  //   val newPrepositions = lowerTokens.filter(isPreposition)
  //   val prepositionBigrams = lowerTokens.sliding(2)
  //     .filter(_.forall(isPreposition))
  //     .map(_.mkString(" ").lowerCase)

  //   val chosenPrepositions: Set[LowerCaseString] = (
  //     newPrepositions.iterator ++
  //       prepositionBigrams ++
  //       Slots.mostCommonPrepositions.iterator).toSet

  //   NonEmptyList.of("", chosenPrepositions.toSeq.map(" " + _): _*)
  // }

  // def getVerbSlotChoices(
  //   forms: InflectedForms
  // ): NonEmptyList[String] = {
  //   import forms._
  //   NonEmptyList.of(
  //     stem.toString,
  //     present.toString,
  //     presentParticiple.toString,
  //     s"be $presentParticiple",
  //     s"been $presentParticiple",
  //     s"have been $presentParticiple",
  //     past.toString,
  //     s"being $pastParticiple",
  //     s"be $pastParticiple",
  //     s"been $pastParticiple",
  //     s"have $pastParticiple",
  //     s"have been $pastParticiple"
  //   ).map(" " + _)
  // }

}
