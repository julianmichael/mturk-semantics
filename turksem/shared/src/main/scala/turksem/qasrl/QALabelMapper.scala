package turksem.qasrl

import cats.implicits._
import turksem.util._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

object QALabelMapper {
  // should always return a result for a properly QA-SRL formatted question
  def getLabelForQuestion(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    question: String
  ): Option[LowerCaseString] = {
    val template = new QASRLStatefulTemplate(new TemplateStateMachine(sentenceTokens, verbInflectedForms))
    template.processStringFully(question) match {
      case Left(QASRLStatefulTemplate.AggregatedInvalidState(_, _)) => None
      case Right(goodStates) =>
        val framesWithAnswerSlots = goodStates.toList.collect {
          case QASRLStatefulTemplate.Complete(
            _, TemplateStateMachine.FrameState(Some(whWord), prepOpt, answerSlotOpt, frame)
          ) if (prepOpt.nonEmpty == frame.args.get(Obj2).nonEmptyAnd(_.isPrep)) &&
              (!Set("who", "what").map(_.lowerCase).contains(whWord) || answerSlotOpt.nonEmpty) =>
            (frame, answerSlotOpt.getOrElse(Adv(whWord)))
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
