package turksem.qasrl

import qasrl._

import cats.implicits._
import turksem.util._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

object QALabelMapper {
  // should always return a result for a properly QA-SRL formatted question
  def getAllLabelsForQuestion(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    question: String
  ): Set[LowerCaseString] = {
    val stateMachine = new TemplateStateMachine(sentenceTokens, verbInflectedForms)
    val template = new QuestionProcessor(stateMachine)
    template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => Set.empty[LowerCaseString]
      case Right(goodStates) =>
        val framesWithAnswerSlots = goodStates.toList.collect {
          case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
        }.toSet
        val labels = framesWithAnswerSlots.toList.map {
          case (frame, Adv(whWord)) => whWord
          case (frame, Obj2) => frame.args.get(Obj2).get match {
            case Prep(preposition, _) => preposition
            case Noun(isAnimate) => (if(isAnimate) "obj-dative" else "obj").lowerCase // obj/2 ambiguous
            case Locative => "where".lowerCase
          }
          case (frame, Obj) => frame.args.get(Obj).get match {
            case Noun(isAnimate) =>
              if(frame.args.get(Obj2).nonEmptyAnd(_.isNoun) || frame.isPassive) { // obj/2 ambiguous
                (if(isAnimate) "obj-dative" else "obj").lowerCase
              } else "obj".lowerCase
          }
          case (frame, Subj) => frame.args.get(Subj).get match {
            case Noun(isAnimate) =>
              if(frame.isPassive) { // obj/2 ambiguous
                (if(frame.args.get(Obj).nonEmpty) "obj-dative" else "obj").lowerCase
              } else if(frame.args.get(Obj).nonEmpty) {
                "subj-transitive".lowerCase
              } else "subj-intransitive".lowerCase
          }
        }
        val res = labels.toSet.flatMap((label: LowerCaseString) =>
          if(stateMachine.prepositionBigrams.contains(label)) {
            label.split(" ").map(_.lowerCase).toSet
          } else Set(label)
        )
        if(res.isEmpty) {
          println
          println(s"Uh oh, discrete labeling was empty!")
          println(s"Question: $question")
          println
        } else if(res.size > 1){
          println(question)
          println("\t" + res.toList.sortBy(_.toString).mkString("; "))
        }
        res
    }
  }

  // should always return a result for a properly QA-SRL formatted question
  def getLabelForQuestion(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    question: String
  ): Option[LowerCaseString] = {
    val template = new QuestionProcessor(new TemplateStateMachine(sentenceTokens, verbInflectedForms))
    template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
      case Right(goodStates) =>
        val framesWithAnswerSlots = goodStates.toList.collect {
          case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
        }.toSet
        val labels = framesWithAnswerSlots.toList.map {
          case (frame, Adv(whWord)) => whWord
          case (frame, Obj2) => frame.args.get(Obj2).get match {
            case Prep(preposition, _) => preposition
            case Noun(isAnimate) => (if(isAnimate) "obj-dative" else "obj").lowerCase // obj/2 ambiguous
            case Locative => "where".lowerCase
          }
          case (frame, Obj) => frame.args.get(Obj).get match {
            case Noun(isAnimate) =>
              if(frame.args.get(Obj2).nonEmptyAnd(_.isNoun) || frame.isPassive) { // obj/2 ambiguous
                (if(isAnimate) "obj-dative" else "obj").lowerCase
              } else "obj".lowerCase
          }
          case (frame, Subj) => frame.args.get(Subj).get match {
            case Noun(isAnimate) =>
              if(frame.isPassive) { // obj/2 ambiguous
                (if(frame.args.get(Obj).nonEmpty) "obj-dative" else "obj").lowerCase
              } else if(frame.args.get(Obj).nonEmpty) {
                "subj-transitive".lowerCase
              } else "subj-intransitive".lowerCase
          }
        }
        val res = labels.modes.headOption
        if(res.isEmpty) {
          println
          println(s"Uh oh, discrete labeling was empty!")
          println(s"Question: $question")
        }
        res
    }
  }
}
