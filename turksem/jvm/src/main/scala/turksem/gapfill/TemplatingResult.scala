package turksem.gapfill

import qamr._

import monocle.macros.Lenses

@Lenses case class TemplatingResult[SID](
  previousAlignments: Map[QAPairId[SID], QuestionTemplateAlignment[SID, TriggerSlot]],
  resultAlignments: Map[QAPairId[SID], QuestionTemplateAlignment[SID, TriggerSlot]],
  unalignedQuestions: Map[QAPairId[SID], SourcedQA[SID]]
) {

  lazy val previousTemplates = previousAlignments.map(_._2.template).toSet
  lazy val resultTemplates = resultAlignments.map(_._2.template).toSet

  lazy val newQuestionAlignments = resultAlignments.filterNot(pair => previousAlignments.contains(pair._1))
  lazy val lostQuestionAlignments = previousAlignments.filterNot(pair => resultAlignments.contains(pair._1))

  lazy val newTemplateAlignments = resultAlignments
    .filterNot { case (id, qta) => previousTemplates.contains(qta.template) }

  lazy val lostTemplateAlignments = previousAlignments
    .filterNot { case (id, qta) => resultTemplates.contains(qta.template) }

  lazy val totalNumQuestions = resultAlignments.size + unalignedQuestions.size

  def proportionQAsCovered = resultAlignments.size.toDouble / totalNumQuestions

  def proportionQAsWithNewTemplate = newTemplateAlignments.size.toDouble / totalNumQuestions

}

object TemplatingResult {

  def initFromQuestions[SID](sqasById: Map[QAPairId[SID], SourcedQA[SID]]) = TemplatingResult(
    previousAlignments = Map.empty[QAPairId[SID], QuestionTemplateAlignment[SID, TriggerSlot]],
    resultAlignments = Map.empty[QAPairId[SID], QuestionTemplateAlignment[SID, TriggerSlot]],
    unalignedQuestions = sqasById)

}
