package turksem.iqa

import cats.implicits._

import turksem.util.InflectionalWord

import nlpdata.util.LowerCaseStrings._

case class CountBasedQuestionGuesser(
  templatePseudoCounts: Map[Template, Double]
  // templateCorrespondencePseudoCounts: Map[TemplateArgumentCorrespondence, Double]
) {
  def guessForTrigger(
    state: QuestioningState,
    triggerWord: InflectionalWord
  ): Option[TemplatedQuestion] = state.triggerGroups.find(_.trigger == triggerWord).flatMap {
    case TriggerGroup(_, templates, qas) =>

      val prepositions = state.sentence
        .map(_.token.lowerCase)
        .filter(CountBasedQuestionGuesser.lotsOfPrepositions.contains)
        .toSet// ++ mostCommonPrepositions

      val templatesAlreadySuccessfullyUsedWithPrepositions = qas
        .filter(_.judgment.isAnswer)
        .map(tq => tq.question.template -> tq.question.getPrepositionSpecifiers)
        .toSet
      val templatedQuestionsInvalid = qas
        .filter(qa => !qa.judgment.isAnswer)
        .map(_.question)
        .toSet

      val candidateArguments = qas.zipWithIndex.collect {
        case (qa, qaIndex) if qa.judgment.isAnswer =>
          Some(qa.question.template.arguments(qa.question.targetIndex) -> qaIndex)
      }.toVector :+ None

      val questions = templates
        .flatMap { case (template, triggerIndex) =>
        template.arguments.zipWithIndex.foldM[List, Vector[Option[ArgumentSpecifier]]](Vector.empty[Option[ArgumentSpecifier]]) {
          case (as, (arg, i)) =>
            val result = (as, (arg, i)) match {
              case (args, (AlignedVerb(_), _)) => List(args :+ Some(SentenceWord(triggerWord.index))) // must be trigger
              case (args, (Adjective(kind), argIndex)) if argIndex == triggerIndex =>
                List(args :+ Some(SentenceWord(triggerWord.index)))
              case (args, (Adjective(kind), i)) =>
                // don't bother checking for existence in prev args bc we only allow one adjective anyway
                state.sentence
                  .filter(_.pos == kind.pos)
                  .map(adj => args :+ Some(SentenceWord(adj.index)))
                  .toList
              case (args, (Preposition, i)) =>
                prepositions
                  .map(p => Some(ExternalWord(p)))
                  .filterNot(args.contains)
                  .map(args :+ _)
                  .toList
              case (args, (n @ Noun(_), i)) =>
                candidateArguments.collect {
                  case Some((Noun(_), index)) => Some(Entity(index))
                  case None => None } // assume DetOrVerb right now
                  .filterNot(args.contains)
                  .map(args :+ _)
                  .toList
              case (args, (ToVerb, _)) => candidateArguments.collect {
                case Some((ToVerb, index)) => Some(Entity(index))
                case None => None }
                  .filterNot(args.contains)
                  .map(args :+ _)
                  .toList
            }

            if(triggerWord.index == 5 && template.toString == "Template(List(Noun(DetOrVerb), Copula, Adjective(Regular), Preposition, Noun(DetOrVerb)))") {
              // println
              // println("CHECKPOINT")
              // println(s"Template: $template")
              // println(s"Arguments: ${template.arguments}")
              // println(s"Chosen args so far: $as")
              // println(s"Next argument: $arg ($i)")
              // println(s"Results:")
              // println("==")
              // result.foreach(println)
              // println("==")
            }
            result
        }.filter { newArgChoices =>
          if(newArgChoices.size != template.arguments.size) {
            // println("WARNING: trying to generate bad argument list")
            // println("Template: " + template)
            // println("Index of trigger in template: " + triggerIndex)
            // println("Arguments: " + template.arguments)
            // println("Argument choices: " + newArgChoices)
          }
          newArgChoices.size == template.arguments.size && newArgChoices.contains(None)
        }.map { args =>
          // println("FINAL:")
          // println("template: " + template.show)
          // println("args: " + args)
          // println
          val targetIndex = args.indexOf(None)
          TemplatedQuestion(
            template, targetIndex,
            TenseAspect.simplePast.copy(tense = PresentTense),
            args.flatten.toList
          )
        }.filterNot(tq =>
          templatedQuestionsInvalid.contains(tq) ||
            templatesAlreadySuccessfullyUsedWithPrepositions.contains(
              tq.template -> tq.getPrepositionSpecifiers)
        )
      }
      // if(questions.isEmpty) None
      // else questions.map { question =>

      // }.maxBy(_._2)._1
      questions.map(q => q -> templatePseudoCounts(q.template)).sortBy(-_._2).headOption.map(_._1)
  }

  def update(
    sentence: Vector[InflectionalWord],
    qas: List[(InflectionalWord, TemplatedQA)]
  ) = {
    this
  }
}

object CountBasedQuestionGuesser {
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

  implicit val adaptiveQuestionGuesser: AdaptiveQuestionGuesser[CountBasedQuestionGuesser] =
    new AdaptiveQuestionGuesser[CountBasedQuestionGuesser] {

      override def guessForTrigger(
        a: CountBasedQuestionGuesser,
        state: QuestioningState,
        triggerWord: InflectionalWord
      ) = None // a.guessForTrigger(state, triggerWord)

      override def update(
        a: CountBasedQuestionGuesser,
        sentence: Vector[InflectionalWord],
        qas: List[(InflectionalWord, TemplatedQA)]
      ) = a // a.update(sentence, qas)

      override def empty: CountBasedQuestionGuesser = CountBasedQuestionGuesser(
        Map.empty[Template, Double]
        // Map.empty[TemplateArgumentCorrespondence, Double]
      )
    }
}

case class TemplateArgumentCorrespondence(
  templates: (Template, Template),
  argumentIndices: (Int, Int)
)
object TemplateArgumentCorrespondence {
  def make(
    templates: (Template, Template),
    argumentIndices: (Int, Int)
  ) = if(templates._1.show < templates._2.show) {
    TemplateArgumentCorrespondence(templates, argumentIndices)
  } else {
    TemplateArgumentCorrespondence(templates.swap, argumentIndices.swap)
  }

  def generateAllPossibleCorrespondences(
    t1: Template, t1TriggerIndex: Int,
    t2: Template, t2TriggerIndex: Int
  ): List[TemplateArgumentCorrespondence] = for {
    (arg1, arg1Index) <- t1.arguments.zipWithIndex
    if arg1Index != t1TriggerIndex
    (arg2, arg2Index) <- t2.arguments.zipWithIndex
    if arg2Index != t2TriggerIndex && arg1 == arg2
  } yield TemplateArgumentCorrespondence.make(t1 -> t2, arg1Index -> arg2Index)
}
