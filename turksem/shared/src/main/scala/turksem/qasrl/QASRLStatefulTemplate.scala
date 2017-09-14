package turksem.qasrl

import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

import TemplateStateMachine.TemplateState
import TemplateStateMachine.FrameState

class QASRLStatefulTemplate(stateMachine: TemplateStateMachine) {

  import QASRLStatefulTemplate._

  def getStatesFromTransition(
    textSoFarReversed: List[Char],
    frameState: FrameState,
    newState: TemplateState
  ): List[ValidState] = newState match {
    case TemplateStateMachine.TemplateComplete =>
      List(Complete(textSoFarReversed.reverse.mkString, frameState))
    case TemplateStateMachine.TemplateProgress(transitions) => transitions.toList.flatMap {
      case (token, nextStateProcessor) => nextStateProcessor.run(frameState) match {
        case None => Nil
        case Some((newFrameState, nextState)) => NonEmptyList.fromList(token.toList) match {
          case None => getStatesFromTransition(textSoFarReversed, newFrameState, nextState)
          case Some(chars) => List(
            InProgressState(textSoFarReversed, newFrameState, chars, nextState)
          )
        }
      }
    }
  }

  def processCharacter(state: ValidState, observedChar: Char): ProcessingState = state match {
    case c @ Complete(str, _) => EitherT.left[List, InvalidState, ValidState](List(InvalidState(c, str.size)))
    case ips @ InProgressState(textSoFarReversed, frameState, textRemainingInCurrentTransition, targetState) =>
      val expectedChar = textRemainingInCurrentTransition.head
      if(expectedChar.toLower != observedChar.toLower) {
        EitherT.left[List, InvalidState, ValidState](List(InvalidState(ips, textSoFarReversed.size)))
      } else {
        val newTextReversed = expectedChar :: textSoFarReversed
        NonEmptyList.fromList(textRemainingInCurrentTransition.tail) match {
          case None => EitherT.right[List, InvalidState, ValidState](
            getStatesFromTransition(newTextReversed, frameState, targetState)
          )
          case Some(remainingChars) =>
            EitherT.right[List, InvalidState, ValidState](
              List(
                InProgressState(newTextReversed, frameState, remainingChars, targetState)
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

    input.toList.foldLeft(
      EitherT.right[List, InvalidState, ValidState](
        getStatesFromTransition(Nil, stateMachine.initialFrameState, stateMachine.start)
      )) {
      case (acc, char) =>
        acc.flatMap(processCharacter(_, char))

        // val firstNecessarilyInvalidCharOpt = acc.value.toList.collect {
        //   case Left(InvalidState(_, n)) => n
        // }.maximumOption

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
        // so now we know validStates is empty
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
    state.targetState == TemplateStateMachine.TemplateComplete

}

object QASRLStatefulTemplate {

  type ProcessingState = EitherT[List, InvalidState, ValidState]

  sealed trait ValidState {
    def fullText: String
    def isComplete: Boolean
    def frameState: FrameState
  }
  case class Complete(
    override val fullText: String,
    override val frameState: FrameState
  ) extends ValidState {
    def isComplete = true
  }
  case class InProgressState(
    textSoFarReversed: List[Char],
    override val frameState: FrameState,
    textRemainingInCurrentTransition: NonEmptyList[Char],
    targetState: TemplateStateMachine.TemplateState
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
