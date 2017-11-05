package example.qpose

import cats.Show
import cats.implicits._

import nlpdata.util.Text
import nlpdata.util.HasTokens.ops._

import example.emnlp2017.SentenceId

import turksem.gapfill.QuestionTemplateAlignment
import turksem.gapfill.TriggerSlot
import turksem.util._

trait PackagePlatformExtensions {
  implicit val directedTemplateAlignmentShowForAlignment: Show[DirectedTemplateAlignment[QuestionTemplateAlignment[SentenceId, TriggerSlot]]] = new Show[DirectedTemplateAlignment[QuestionTemplateAlignment[SentenceId, TriggerSlot]]] {
    override def show(dta: DirectedTemplateAlignment[QuestionTemplateAlignment[SentenceId, TriggerSlot]]): String = {
      val sourceTemplateStr =  dta.source.template.mapWithIndex((sl: TriggerSlot, i: Int) => s"${sl.label}-$i").show + " / A"
      val targetTemplateStr = dta.target.template.mapWithZippedList(
        dta.questionAlignments, (sl: TriggerSlot, iOpt: Option[TemplateAlignmentIndex]) => s"${sl.label}-${iOpt.get.show}"
      ).show + dta.answerAlignmentOpt.foldMap(i => " " + i.show)
      val sourceSingleWordAnswers = dta.source.sourcedQA.answers.filter(_.size == 1).map(_.head).map(dta.source.sourcedQA.id.sentenceId.tokens(_)).toSet.mkString(" / ")
      val sourceQA = dta.source.sourcedQA.wqa.question + " " + sourceSingleWordAnswers
      f"$sourceTemplateStr%-35s --> $targetTemplateStr%-35s $sourceQA%-50s --> ${dta.target.sourcedQA.wqa.question}%-40s ${Text.render(dta.source.sourcedQA.id.sentenceId)}"
    }
  }
}
