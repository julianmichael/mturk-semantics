package example.emnlp2017.silly

import turksem.util.ContiguousSpan

import nlpdata.util.LowerCaseStrings._

import cats.implicits._

case class TemplatedQA(
  question: TemplatedQuestion,
  judgment: QuestionJudgment)

sealed trait QuestionJudgment {
  def getAnswer = this match {
    case a @ Answer(_, _) => Some(a)
    case _ => None
  }
  def isAnswer = getAnswer.nonEmpty
}
case class Answer(
  answerSpan: ContiguousSpan,
  standInAnswer: String
) extends QuestionJudgment
case object NoAnswer extends QuestionJudgment
case object BadQuestion extends QuestionJudgment

case class TriggerGroup(
  trigger: InflectionalWord,
  templates: List[(Template, Int)],
  qas: Map[Int, TemplatedQA]) { // map from original index
  // TODO calculate alignments
}

case class QuestionsState(
  sentence: Vector[InflectionalWord],
  qas: Vector[TemplatedQA]
) {
  val entityStrings = qas.zipWithIndex.flatMap {
    case (qa, index) => qa.judgment.getAnswer.map(index -> _)
  }.toMap

  val triggerGroups = sentence.flatMap { w =>
    templates.getGroupForPosTag(w.pos).map { triggerTemplates =>
      val qasForTrigger = qas.zipWithIndex.filter { case (tqa, _) =>
        templates.getTriggerIndex(tqa.question.template).fold(false) { triggerIndex =>
          tqa.question.arguments(triggerIndex) match {
            case SentenceWord(i) if i == w.index => true
            case _ => false
          }
        }
      }.map { case (tqa, index) => index -> tqa }.toMap

      TriggerGroup(w, triggerTemplates, qasForTrigger)
    }
  }
}

object SimpleQuestionGuesser extends QuestionGuesser {

  // use all prepositions as "external words"

  val mostCommonPrepositions = Set(
    "by", "for", "with", "in", "from", "to", "as"
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

  // simplest-ass guessing function I can come up with
  // later we want to incorporate lots of statistics and knowledge into this
  // right now only guessing trigger questions
  final def guesses(state: QuestionsState): List[TemplatedQuestion] = {
    // TODO add more prepositions when we can be smarter about it
    val prepositions = state.sentence
      .map(_.token.lowerCase)
      .filter(lotsOfPrepositions.contains)
      .toSet// ++ mostCommonPrepositions
    state.triggerGroups.toList.flatMap { case TriggerGroup(trigger, templates, qas) =>
      val answersInGroup = qas.collect {
        case (qaIndex, qa) if qa.judgment.isAnswer =>
          qa.question.template.arguments(qa.question.targetIndex) -> qaIndex
      }
      val templateInstantiations = templates.flatMap { case (template, triggerIndex) =>
        val existingInstantiations = qas.collect {
          case (qaIndex, qa) if qa.question.template == template =>
            qa.judgment.getAnswer.fold(qa.question.argsWithAnswerPlaceholder)(_ =>
              qa.question.reconstructedTemplateArgs(Entity(qaIndex)).map(Option(_))
            )
        }
        template.arguments.zipWithIndex.foldM(Vector.empty[ArgumentSpecifier]) {
          case (args, (AlignedVerb(_), _)) => List(args :+ SentenceWord(trigger.index)) // must be trigger
          case (args, (Adjective(_), argIndex)) if argIndex == triggerIndex => List(args :+ SentenceWord(trigger.index))
          case (args, (Adjective(kind), _)) =>
            // don't bother checking for existence in prev args bc we only allow one adjective anyway
            state.sentence.filter(_.pos == kind.pos).map(adj => args :+ SentenceWord(adj.index)).toList
          case (args, (Preposition, _)) => prepositions
              .map(p => ExternalWord(p))
              .filterNot(args.contains)
              .map(args :+ _)
              .toList
          case (args, (Noun(_), _)) => answersInGroup
              .collect { case (Noun(_), index) => Entity(index) } // assume DetOrVerb right now
              .filterNot(args.contains)
              .map(args :+ _)
              .toList
          case (args, (ToVerb, _)) => answersInGroup
              .collect { case (ToVerb, index) => Entity(index) }
              .filterNot(args.contains)
              .map(args :+ _)
              .toList
        }.filterNot { newArgChoices =>
          existingInstantiations.exists { prevArgChoices =>
            (prevArgChoices, newArgChoices).zipped.forall {
              case (Some(x), y) => x == y
              case _ => true
            }
          }
        }.map(template -> _)
      }
      val questions = templateInstantiations.flatMap { case (template, args) =>
        args.indices.map { targetIndex =>
          TemplatedQuestion(
            template, targetIndex,
            TenseAspect.simplePast.copy(tense = PresentTense),
            (args.take(targetIndex) ++ args.drop(targetIndex + 1)).toList
          )
        }
      }
      questions.sortBy(_.arguments.size)
    }

  }
}

trait QuestionGuesser {
  def guesses(state: QuestionsState): List[TemplatedQuestion]
}
