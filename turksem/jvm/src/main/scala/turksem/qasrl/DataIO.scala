package turksem.qasrl

import cats.implicits._

import turkey.HITInfo

import nlpdata.util.HasTokens.ops._
import nlpdata.util.HasTokens
import nlpdata.util.Text

import turksem.util.ContiguousSpan

object DataIO {

  def makeReadableQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]],
    keepQA: (SID, VerbQA, List[QASRLValidationAnswer]) => Boolean = (
      (_: SID, _: VerbQA, _: List[QASRLValidationAnswer]) => true)
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = true // for now, print everything
      sentenceSB.append(s"${idString}\t${nlpdata.util.Text.render(sentenceTokens)}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))
            if(valResponses.size != 2) {
              System.err.println("Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size)
            }
            val valAnswers = valResponses.map(_._2)

            if(keepQA(id, wqa, valAnswers)) {
              shouldIncludeSentence = true
              sentenceSB.append(anonymizeWorker(genAssignment.workerId) + "\t") // anonymized worker ID
              sentenceSB.append(s"${Text.normalizeToken(sentenceTokens(wqa.verbIndex))} (${wqa.verbIndex})\t")
              sentenceSB.append(wqa.question + "\t") // question string written by worker
              sentenceSB.append(
                ((Answer(wqa.answers)) :: valResponses.map(_._2)).map { valAnswer =>
                  QASRLValidationAnswer.render(sentenceTokens, valAnswer, genAssignment.response)
                }.mkString("\t")
              )
              sentenceSB.append("\n")
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }
}
