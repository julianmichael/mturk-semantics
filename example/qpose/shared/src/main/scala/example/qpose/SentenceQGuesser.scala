package example.qpose

import cats.Show
import cats.implicits._

import turksem.gapfill.Answer
import turksem.gapfill.InstantiatedQuestion
import turksem.gapfill.JudgedQuestion
import turksem.gapfill.QuestionGuesser
import turksem.gapfill.QuestioningState
import turksem.gapfill.QuestionTemplate
import turksem.gapfill.QuestionTemplateAlignment
import turksem.gapfill.SentenceWord
import turksem.gapfill.TriggerSlot
import turksem.gapfill.PreviousAnswerSpan

import turksem.util._

import example.emnlp2017.SentenceId

import nlpdata.util.HasTokens.ops._

sealed trait TemplateAlignmentIndex
case class QuestionSlotIndex(i: Int) extends TemplateAlignmentIndex
case object AnswerIndex extends TemplateAlignmentIndex
object TemplateAlignmentIndex {
  implicit val templateAlignmentIndexShow: Show[TemplateAlignmentIndex] = new Show[TemplateAlignmentIndex] {
    override def show(tai: TemplateAlignmentIndex): String = tai match {
      case QuestionSlotIndex(i) => s"$i"
      case AnswerIndex => "A"
    }
  }
}

// expect A to be QuestionTemplate[TriggerSlot] or QuestionTemplateAlignment[SentenceId, TriggerSlot]
sealed trait DirectedTemplateAlignment[A] extends Product with Serializable {
  def source: A
  def target: A
  def questionAlignments: List[TemplateAlignmentIndex]

  def answerAlignmentOpt: Option[TemplateAlignmentIndex]

  final def map[B](f: A => B) = this match {
    case ParaphraseAlignment(source, target, qas, answerAlignmentOpt) =>
      ParaphraseAlignment(f(source), f(target), qas, answerAlignmentOpt)
    case DirectToQuestionAlignment(source, target, qas) =>
      DirectToQuestionAlignment(f(source), f(target), qas)
  }
}
// same number of slots between source and target
case class ParaphraseAlignment[A](
  override val source: A,
  override val target: A,
  override val questionAlignments: List[TemplateAlignmentIndex],
  override val answerAlignmentOpt: Option[TemplateAlignmentIndex] // None in case of a new/unseen answer
) extends DirectedTemplateAlignment[A] {
  def isDirectParaphrase = !questionAlignments.contains(AnswerIndex)
  def answersAgree = answerAlignmentOpt.nonEmptyAnd(_ == AnswerIndex)
}
// target has one more slot than source
case class DirectToQuestionAlignment[A](
  override val source: A,
  override val target: A,
  override val questionAlignments: List[TemplateAlignmentIndex]
) extends DirectedTemplateAlignment[A] {
  override val answerAlignmentOpt = None
}

object DirectedTemplateAlignment {

  implicit val directedTemplateAlignmentShowForTemplate: Show[DirectedTemplateAlignment[QuestionTemplate[TriggerSlot]]] = new Show[DirectedTemplateAlignment[QuestionTemplate[TriggerSlot]]] {
    override def show(dta: DirectedTemplateAlignment[QuestionTemplate[TriggerSlot]]): String = {
      val sourceTemplateStr =  dta.source.mapWithIndex((sl: TriggerSlot, i: Int) => s"${sl.label}-$i").show + " / A"
      val targetTemplateStr = dta.target.mapWithZippedList(
        dta.questionAlignments, (sl: TriggerSlot, iOpt: Option[TemplateAlignmentIndex]) => s"${sl.label}-${iOpt.get.show}"
      ).show + dta.answerAlignmentOpt.foldMap(i => " " + i.show)
      f"$sourceTemplateStr%-35s --> $targetTemplateStr%-35s"
    }
  }
}

case class NextQuestionProbabilities(
  source: QuestionTemplate[TriggerSlot],
  sourceFrequency: Int,
  targetFrequencies: Map[DirectToQuestionAlignment[QuestionTemplate[TriggerSlot]], Int]) {
  def getTargetProbability(target: DirectToQuestionAlignment[QuestionTemplate[TriggerSlot]]) =
    targetFrequencies(target).toDouble / sourceFrequency
  def getTargetLogProbability(target: DirectToQuestionAlignment[QuestionTemplate[TriggerSlot]]) =
    math.log(targetFrequencies.get(target).getOrElse(1).toDouble / sourceFrequency)
}
object NextQuestionProbabilities {
}

case class SentenceQGuesser(
  sentenceTokens: Vector[InflectionalWord],
  starterTemplateProbsByCoarseGrainedLabel: Map[String, List[(QuestionTemplate[TriggerSlot], Double)]],
  questionExpansionProbabilities: Map[QuestionTemplate[TriggerSlot], NextQuestionProbabilities]
) {
  def guessForTrigger(state: QuestioningState, triggerGroupIndex: Int): Option[InstantiatedQuestion] = {
    val triggerGroup = state.triggerGroups(triggerGroupIndex)
    val allQuestionStrings = state.triggerGroups.flatMap(_.qas).map(qa => state.renderQuestion(qa.question)).toSet
    val initLogProbs = starterTemplateProbsByCoarseGrainedLabel(triggerGroup.triggerLabel).map {
      case (template, prob) => InstantiatedQuestion(template, List(SentenceWord(triggerGroup.trigger.index))) -> math.log(prob)
    }
    val expansionLogProbs = expandedQuestionLogProbabilities(state)
    val expansionInterpolationFactor = state.triggerGroups.size.toDouble / (state.triggerGroups.size + 1)
    val initInterpolationFactor = 1.0 - expansionInterpolationFactor
    def fullRescaledProbsIter = initLogProbs.iterator.map {
      case (iq, logProb) => iq -> (logProb + math.log(initInterpolationFactor))
    } ++ expansionLogProbs.iterator.map {
      case (iq, logProb) => iq -> (logProb + math.log(expansionInterpolationFactor))
    }
    // TODO scale by probability that the question is not covered yet
    fullRescaledProbsIter.filter {
      case (iq @ InstantiatedQuestion(_, args), _) =>
        args.contains(SentenceWord(triggerGroup.trigger.index)) &&
          !allQuestionStrings.contains(state.renderQuestion(iq))
    }.toVector.sortBy(-_._2).headOption.map(_._1)
  }

  def expandedQuestionLogProbabilities(state: QuestioningState) = {
    val allTargetQuestions = for {
      (group, groupIndex) <- state.triggerGroups.zipWithIndex
      (JudgedQuestion(InstantiatedQuestion(template, sourceArgs), Answer(_)), questionIndex) <- group.qas.zipWithIndex
      nextQuestionProbabilities <- questionExpansionProbabilities.get(template).toList
      transformation <- nextQuestionProbabilities.targetFrequencies.keys
    } yield {
      // val prob = nextQuestionProbabilities.getTargetLogProbability(transformation.target)
      val targetArgs = transformation.questionAlignments.map {
        case AnswerIndex => PreviousAnswerSpan(groupIndex, questionIndex)
        case QuestionSlotIndex(i) => sourceArgs(i)
      }
      InstantiatedQuestion(transformation.target, targetArgs)
    }
    val probabilitiesOfEachQA = allTargetQuestions.map {
      case iq @ InstantiatedQuestion(target, targetArgs) =>
        val logProbs = for {
          (group, groupIndex) <- state.triggerGroups.zipWithIndex
          (JudgedQuestion(InstantiatedQuestion(source, sourceArgs), Answer(_)), questionIndex) <- group.qas.zipWithIndex
          // assess if source question is relevant to target
          if targetArgs.size == (sourceArgs.size + 1)
          nextQuestionProbabilities <- questionExpansionProbabilities.get(source)
          alignment <- targetArgs.map {
            case PreviousAnswerSpan(`groupIndex`, `questionIndex`) => Some(AnswerIndex)
            case otherArg => sourceArgs.findIndex(_ == otherArg).map(QuestionSlotIndex(_))
          }.sequence
        } yield nextQuestionProbabilities.getTargetLogProbability(
          DirectToQuestionAlignment(source, target, alignment)
        )
        // going to be nonempty (assuming no bugs) because each was generated by at least one question
        iq -> logProbs.sum
    }.groupBy { case (iq, _) =>
        state.renderQuestion(iq)
    }.map { case (_, iqsWithProbs) => iqsWithProbs.head
    }.toVector.sortBy(-_._2)
    probabilitiesOfEachQA
  }
}
object SentenceQGuesser {
  implicit val sentenceQGuesserQuestionGuesser = new QuestionGuesser[SentenceQGuesser] {
    override def guessForTrigger(a: SentenceQGuesser, state: QuestioningState, triggerGroupIndex: Int): Option[InstantiatedQuestion] =
      a.guessForTrigger(state, triggerGroupIndex)
  }

  import upickle.default._
  import upickle.Js
  import nlpdata.util.LowerCaseStrings._
  import turksem.gapfill.LowerCaseStringSerialization._

  implicit def sentenceQGuesserWriter = Writer[SentenceQGuesser] {
    case SentenceQGuesser(x, y, z) =>
      Js.Str(write(x) + "_X_X_X_" + write(y) + "_X_X_X_" + write(z))
  }

  implicit def sentenceQGuesserReader = Reader[SentenceQGuesser] {
    case Js.Str(str) =>
      val Array(x, y, z) = str.split("_X_X_X_")
      SentenceQGuesser(
        read[Vector[InflectionalWord]](x),
        read[Map[String, List[(QuestionTemplate[TriggerSlot], Double)]]](y),
        read[Map[QuestionTemplate[TriggerSlot], NextQuestionProbabilities]](z))
  }
}
