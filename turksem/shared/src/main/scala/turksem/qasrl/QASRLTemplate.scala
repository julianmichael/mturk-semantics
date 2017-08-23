package turksem.qasrl

import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

import Slots.TemplateState

class QASRLTemplate(slots: Slots) {

  import QASRLTemplate._

  def getStatesFromTransition(textSoFarReversed: List[Char], newState: TemplateState): NonEmptyList[ValidState] = newState match {
    case Slots.TemplateComplete => NonEmptyList.of(Complete(textSoFarReversed.reverse.mkString))
    case Slots.TemplateProgress(transitions) => transitions.flatMap {
      case (token, nextState) => NonEmptyList.fromList(token.toList) match {
        case None => getStatesFromTransition(textSoFarReversed, nextState)
        case Some(chars) => NonEmptyList.of(
          InProgressState(textSoFarReversed, chars, nextState)
        )
      }
    }
  }

  def processCharacter(state: ValidState, observedChar: Char): ProcessingState = state match {
    case c @ Complete(str) => EitherT.left[NonEmptyList, InvalidState, ValidState](NonEmptyList.of(InvalidState(c, str.size)))
    case ips @ InProgressState(textSoFarReversed, textRemainingInCurrentTransition, targetState) =>
      val expectedChar = textRemainingInCurrentTransition.head
      if(expectedChar.toLower != observedChar.toLower) {
        EitherT.left[NonEmptyList, InvalidState, ValidState](NonEmptyList.of(InvalidState(ips, textSoFarReversed.size)))
      } else {
        val newTextReversed = expectedChar :: textSoFarReversed
        NonEmptyList.fromList(textRemainingInCurrentTransition.tail) match {
          case None => EitherT.right[NonEmptyList, InvalidState, ValidState](
            getStatesFromTransition(newTextReversed, targetState)
          )
          case Some(remainingChars) =>
            EitherT.right[NonEmptyList, InvalidState, ValidState](
              NonEmptyList.of(
                InProgressState(newTextReversed, remainingChars, targetState)
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

    input.toList.foldLeft(EitherT.right[NonEmptyList, InvalidState, ValidState](getStatesFromTransition(Nil, slots.start))) {
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

  // TODO move
  def isAlmostComplete(state: InProgressState) =
    state.targetState == Slots.TemplateComplete

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
    textRemainingInCurrentTransition: NonEmptyList[Char],
    targetState: Slots.TemplateState
  ) extends ValidState {
    def fullText = textSoFarReversed.reverse.mkString + textRemainingInCurrentTransition.toList.mkString
    def isComplete = false
  }

  case class InvalidState(
    lastGoodState: ValidState,
    numGoodCharacters: Int)

  case class AggregatedInvalidState(
    lastGoodStates: NonEmptyList[ValidState],
    numGoodCharacters: Int)

}
