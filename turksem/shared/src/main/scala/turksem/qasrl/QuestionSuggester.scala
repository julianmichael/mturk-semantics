package turksem.qasrl

import qasrl._
import qasrl.crowd._
import qasrl.util.DependentMap

import cats.Id
import cats.data.NonEmptyList
import cats.implicits._

import turksem.util._

import nlpdata.util.LowerCaseStrings._

object QuestionSuggester {
  def getSuggestionsForNegativeSampling(
    template: QuestionProcessor,
    questions: List[String],
    rand: scala.util.Random,
    whDist: CategoricalDistribution[LowerCaseString],
    // prepDist: CategoricalDistribution[LowerCaseString], // TODO
    admissibleFrameSpecs: List[(DependentMap[ArgumentSlot.Aux, Id], Boolean)],
    numNegativeSamples: Int
  ): List[String] = {
    val allLowercaseQuestionStrings = questions.map(_.lowerCase).toSet
    val initFramesWithAnswerSlots: List[(Frame, ArgumentSlot)] =
      questions.flatMap(question =>
        template.processStringFully(question) match {
          case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => Nil
          case Right(goodStates) => goodStates.toList.collect {
            case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
          }
        }
      )
    val admissibleFramesWithAnswerSlots = initFramesWithAnswerSlots.filter(p =>
      admissibleFrameSpecs.contains(p._1.args, p._1.isPassive)
    )
    val framesWithAnswerSlots =
      if(admissibleFramesWithAnswerSlots.nonEmpty) admissibleFramesWithAnswerSlots
      else initFramesWithAnswerSlots
    val frameCounts = counts(framesWithAnswerSlots.map(_._1))
    val frameToFilledAnswerSlots = framesWithAnswerSlots.foldLeft(Map.empty[Frame, Set[ArgumentSlot]].withDefaultValue(Set.empty[ArgumentSlot])) {
      case (acc, (frame, slot)) => acc.updated(frame, acc(frame) + slot)
    }
    val framesByCountDecreasing = frameCounts.keys.toList.sortBy(f => -10 * frameCounts(f) + math.abs(f.args.size - 2))
    val usedAdvSlots = framesWithAnswerSlots.collect {
      case (_, Adv(wh)) => Adv(wh)
    }.toSet
    val allQuestions = framesByCountDecreasing.flatMap { frame =>
      // assume these are answered by other frames---they basically always are
      // val unAnsweredSlots = frame.args.keys.toSet -- frameToFilledAnswerSlots(frame)
      // val coreArgQuestions = unAnsweredSlots.toList.flatMap(frame.questionsForSlot)
      // val newPrepositionQuestions = // TODO
      val advQuestions = ArgumentSlot.allAdvSlots
        .filterNot(usedAdvSlots.contains)
        .flatMap(frame.questionsForSlot)
      advQuestions
    }.filterNot(q => allLowercaseQuestionStrings.contains(q.lowerCase)).distinct
    val allQuestionSuggestions = allQuestions.flatMap(q =>
      template.processStringFully(q) match {
        case Right(goodStates) if goodStates.exists(_.isComplete) => Some(q)
        case _ => None
      }
    )

    def getNextNegativeSample(remainingSuggestions: NonEmptyList[String], dist: CategoricalDistribution[LowerCaseString]): Option[String] = {
      val chosenWh = dist.sample(rand)
      // TODO if choose who/what, sample preposition question according to preposition probability
      val possibleChoices = if(chosenWh == "how".lowerCase) {
        remainingSuggestions.filter(q =>
          q.toLowerCase.startsWith("how") && !q.toLowerCase.startsWith("how much")
            && !q.toLowerCase.startsWith("how long")
        )
      } else remainingSuggestions.filter(_.toLowerCase.startsWith(chosenWh.toString))
      NonEmptyList.fromList(possibleChoices).fold {
        dist.filter(_ != chosenWh).flatMap(
          getNextNegativeSample(remainingSuggestions, _)
        )
      }(nel => Some(CategoricalDistribution.uniform(nel).sample(rand)))
    }

    val allAvailableWhs = NonEmptyList.of("who", "what", "when", "where", "why", "how", "how much", "how long").map(_.lowerCase)
    (NonEmptyList.fromList(allQuestionSuggestions), numNegativeSamples).unfoldList[String] {
      case (None, _) => None
      case (_, 0) => None
      case (Some(remainingSuggestions), numSamplesRemaining) if numSamplesRemaining > 0 =>
        getNextNegativeSample(remainingSuggestions, whDist).map(chosenQ =>
          (chosenQ, (NonEmptyList.fromList(remainingSuggestions.filter(_ != chosenQ)), numSamplesRemaining - 1))
        )
    }
    // allQuestionSuggestions
  }
}
