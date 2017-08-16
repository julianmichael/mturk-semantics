package turksem.qasrl

import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

case class QASRLTemplate(
  verbChoices: SlotChoices,
  prepositionChoices: SlotChoices) {

  val templateChoices = Vector(
    Slots.whChoices,
    Slots.auxChoices,
    Slots.subjChoices,
    verbChoices,
    Slots.objChoices,
    prepositionChoices,
    Slots.obj2Choices,
    Slots.questionMark)

  // should process a string just to produce these transitions at the end
  // plus information about completion/bad character index
  case class AccessibleTransition(
    fullToken: String,
    textToAdd: String,
    targetState: Option[Int], // None means complete
    numInvalidCharacters: Int) // number of invalid characters trailing at the end
  // longest common prefix:
  // (x.view, y.view).zipped.takeWhile(p => p._1 == p._2).unzip._1.mkString)
  // x.indices.find(i => x(i) != y(i)).fold(x)(x.substring(0, _))
  // x.zipWithIndex.takeWhile { case (c, i) => c == y(i) }.map(_._1).mkString

  def advanceThroughToken(
    nextIndex: Int,
    token: LowerCaseString
  ): List[Int] = {
    val choices = templateChoices(nextIndex)
    val advanceCurrentOpt = choices.admits(token).ifTrue(Option(nextIndex + 1))
    val skipAndAdvance = choices.canSkip.ifTrue(advanceThroughToken(nextIndex + 1, token))
    advanceCurrentOpt.fold(skipAndAdvance)(_ :: skipAndAdvance)
  }

  def advanceStateThroughToken(
    state: MatchingState,
    nextToken: LowerCaseString,
    nextTokenIndex: Int
  ): MatchingState = state match {
    case Complete => Invalid(Nil, nextTokenIndex)
    case invalid @ Invalid(_, _) => invalid
    case InProgress(nextStates) =>
      NonEmptyList
        .fromList(nextStates.toList.flatMap(advanceThroughToken(_, nextToken)))
        .fold(Invalid(nextStates.toList, nextTokenIndex): MatchingState)(InProgress(_))
  }

  sealed trait MatchingState
  case class InProgress(nextStates: NonEmptyList[Int]) extends MatchingState
  case object Complete extends MatchingState
  case class Invalid(nextStates: List[Int], firstInvalidTokenIndex: Int) extends MatchingState

  def processTokens[F[_]: Foldable](
    tokens: F[LowerCaseString]
  ): MatchingState = tokens
    .toList.zipWithIndex
    .foldLeft(InProgress(NonEmptyList(0, Nil)): MatchingState) {
    case (state, (nextToken, nextTokenIndex)) =>
      advanceStateThroughToken(state, nextToken, nextTokenIndex)
  }

  // def getNextTokens(state: MatchingState): List[String] = state match {
  //   case Complete => Nil
  //   case Invalid(nextStates, _) => getNextTokens(nextStates)
  //   case InProgress(nextStates) => getNextTokens(nextStates)
  // }

  def getNextSlotChoices[F[_] : Foldable](states: F[Int]): Set[SlotChoices] = {
    states.foldLeft(Set.empty[SlotChoices]) {
      case (allSlots, stateIndex) => allSlots + templateChoices(stateIndex)
    }
  }

  def getNextTokens[F[_] : Foldable](states: F[Int]): List[String] = {
    states.foldLeft(Set.empty[String]) {
      case (allWords, stateIndex) => allWords ++ templateChoices(stateIndex).tokens
    }.toList.sortBy(_.toLowerCase)
  }

  def isValid[F[_] : Foldable](tokens: F[LowerCaseString]): Boolean =
    processTokens(tokens) == Complete

}
