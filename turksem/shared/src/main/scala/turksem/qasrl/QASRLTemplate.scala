package turksem.qasrl

import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms


case class QASRLTemplate(
  verbChoices: NonEmptyList[String],
  prepositionChoices: NonEmptyList[String]) {

  import QASRLTemplate._

  val templateChoices = Vector[NonEmptyList[String]](
    Slots.whChoices,
    Slots.auxChoices,
    Slots.subjChoices,
    verbChoices,
    Slots.objChoices,
    prepositionChoices,
    Slots.obj2Choices,
    Slots.questionMark)

  def getStatesFromTransition(textSoFarReversed: List[Char], newState: Int): NonEmptyList[ValidState] = {
    if(newState == templateChoices.size) NonEmptyList.of(Complete(textSoFarReversed.reverse.mkString))
    else templateChoices(newState).flatMap(token =>
      NonEmptyList.fromList(token.toList) match {
        case None => getStatesFromTransition(textSoFarReversed, newState + 1)
        case Some(chars) => NonEmptyList.of(
          InProgressState(textSoFarReversed, chars, newState)
        )
      }
    )
  }

  def processCharacter(state: ValidState, observedChar: Char): ProcessingState = state match {
    case c @ Complete(str) => EitherT.left[NonEmptyList, InvalidState, ValidState](NonEmptyList.of(InvalidState(c, str.size)))
    case ips @ InProgressState(textSoFarReversed, textRemainingInCurrentState, currentStateInProgress) =>
      val expectedChar = textRemainingInCurrentState.head
      if(expectedChar.toLower != observedChar.toLower) {
        EitherT.left[NonEmptyList, InvalidState, ValidState](NonEmptyList.of(InvalidState(ips, textSoFarReversed.size)))
      } else {
        val newTextReversed = expectedChar :: textSoFarReversed
        NonEmptyList.fromList(textRemainingInCurrentState.tail) match {
          case None => EitherT.right[NonEmptyList, InvalidState, ValidState](
            getStatesFromTransition(newTextReversed, currentStateInProgress + 1)
          )
          case Some(remainingChars) =>
            EitherT.right[NonEmptyList, InvalidState, ValidState](
              NonEmptyList.of(
                InProgressState(newTextReversed, remainingChars, currentStateInProgress)
              )
            )
        }
      }
  }

  def processString(input: String): ProcessingState = {
    // TODO WHY IS THIS NOT THE SAME AS BELOW
    // EitherT.right[NonEmptyList, InvalidState, ValidState](getStatesFromTransition(Nil, 0)) >>= (init =>
    //   input.toList.foldM[EitherT[NonEmptyList, InvalidState, ?], ValidState](init)(processCharacter)
    // )

    // the equality I expected:
    // mz >>= (z => l.foldLeftM(z)(f)) == l.foldLeft(mz)((ma, x) => ma >>= (f(_, x)))

    input.toList.foldLeft(EitherT.right[NonEmptyList, InvalidState, ValidState](getStatesFromTransition(Nil, 0))) {
      case (acc, char) =>
        val firstNecessarilyInvalidCharOpt = acc.value.toList.collect {
          case Left(InvalidState(_, n)) => n
        }.maximumOption

        acc.flatMap(processCharacter(_, char))

        // NOTE pre-exludes bad early results for efficiency. could put in another impl. but right now doesn't matter
        // val newAcc = firstNecessarilyInvalidCharOpt.fold(acc) { firstNecessarilyInvalidChar =>
        //   EitherT(
        //     NonEmptyList.fromList(
        //       acc.value.toList.filter {
        //         case Left(InvalidState(_, n)) => n == firstNecessarilyInvalidChar
        //         case _ => true
        //       }).get
        //     )
        // }
        // newAcc.flatMap(processCharacter(_, char))
    }

  }

  def processStringFully(input: String): Either[AggregatedInvalidState, NonEmptyList[ValidState]] = {
    val resultState = processString(input)
    // we know that that at least one of the two parts is nonempty
    val (invalidStates, validStates) = resultState.value.toList.separate
    Either.fromOption(
      NonEmptyList.fromList(validStates), {
        // so now we know invalidStates is empty
        val maxGoodLength = invalidStates.map(_.numGoodCharacters).max
        // and this as well, since something had to have the max
        val lastGoodStates = invalidStates.collect {
          case InvalidState(lastGoodState, `maxGoodLength`) => lastGoodState
        }
        AggregatedInvalidState(NonEmptyList.fromList(lastGoodStates).get, maxGoodLength)
      })
  }

  def isValid(input: String): Boolean =
    processStringFully(input).toOption.exists(_.exists(_.isComplete))

  def isAlmostComplete(state: InProgressState) =
    state.currentStateInProgress == templateChoices.size - 1

  // def processStringAtState(prevString: String, remainingString: LowerCaseString, curState: Int): List[AccessibleTransition] = {
  //   val template = templateChoices(curState)
  //   val prefixes = template.tokens.map(token => longestCommonPrefix(token.toLowerCase, remainingString) -> token)
  //   // 3 cases:
  //   // 1. string contains whole token
  //   // 2. string contains partial or no token
  //   // there can only be one such
  //   val fullyMatchedTokenOpt = prefixes.filter { case (prefix, token) => prefix == token.toLowerCase}
  // }

  // def advanceThroughToken(
  //   nextIndex: Int,
  //   token: LowerCaseString
  // ): List[Int] = {
  //   val choices = templateChoices(nextIndex)
  //   val advanceCurrentOpt = choices.admits(token).ifTrue(Option(nextIndex + 1))
  //   val skipAndAdvance = choices.canSkip.ifTrue(advanceThroughToken(nextIndex + 1, token))
  //   advanceCurrentOpt.fold(skipAndAdvance)(_ :: skipAndAdvance)
  // }

  // def advanceStateThroughToken(
  //   state: MatchingState,
  //   nextToken: LowerCaseString,
  //   nextTokenIndex: Int
  // ): MatchingState = state match {
  //   case Complete => Invalid(Nil, nextTokenIndex)
  //   case invalid @ Invalid(_, _) => invalid
  //   case InProgress(nextStates) =>
  //     NonEmptyList
  //       .fromList(nextStates.toList.flatMap(advanceThroughToken(_, nextToken)))
  //       .fold(Invalid(nextStates.toList, nextTokenIndex): MatchingState)(InProgress(_))
  // }

  // sealed trait MatchingState
  // case class InProgress(nextStates: NonEmptyList[Int]) extends MatchingState
  // case object Complete extends MatchingState
  // case class Invalid(nextStates: List[Int], firstInvalidTokenIndex: Int) extends MatchingState

  // def processTokens[F[_]: Foldable](
  //   tokens: F[LowerCaseString]
  // ): MatchingState = tokens
  //   .toList.zipWithIndex
  //   .foldLeft(InProgress(NonEmptyList(0, Nil)): MatchingState) {
  //   case (state, (nextToken, nextTokenIndex)) =>
  //     advanceStateThroughToken(state, nextToken, nextTokenIndex)
  // }

  // // def getNextTokens(state: MatchingState): List[String] = state match {
  // //   case Complete => Nil
  // //   case Invalid(nextStates, _) => getNextTokens(nextStates)
  // //   case InProgress(nextStates) => getNextTokens(nextStates)
  // // }

  // def getNextSlotChoices[F[_] : Foldable](states: F[Int]): Set[SlotChoices] = {
  //   states.foldLeft(Set.empty[SlotChoices]) {
  //     case (allSlots, stateIndex) => allSlots + templateChoices(stateIndex)
  //   }
  // }

  // def getNextTokens[F[_] : Foldable](states: F[Int]): List[String] = {
  //   states.foldLeft(Set.empty[String]) {
  //     case (allWords, stateIndex) => allWords ++ templateChoices(stateIndex).tokens
  //   }.toList.sortBy(_.toLowerCase)
  // }

  // def isValid[F[_] : Foldable](tokens: F[LowerCaseString]): Boolean =
  //   processTokens(tokens) == Complete

}

object QASRLTemplate {

  type ProcessingState = EitherT[NonEmptyList, InvalidState, ValidState]

  sealed trait ValidState {
    def fullText: String
    def isComplete: Boolean
  }
  case class Complete(override val fullText: String) extends ValidState {
    def isComplete = true
  }
  case class InProgressState(
    textSoFarReversed: List[Char],
    textRemainingInCurrentState: NonEmptyList[Char],
    currentStateInProgress: Int
  ) extends ValidState {
    def fullText = textSoFarReversed.reverse.mkString + textRemainingInCurrentState.toList.mkString
    def isComplete = false
  }

  case class InvalidState(
    lastGoodState: ValidState,
    numGoodCharacters: Int)

  case class AggregatedInvalidState(
    lastGoodStates: NonEmptyList[ValidState],
    numGoodCharacters: Int)

}
