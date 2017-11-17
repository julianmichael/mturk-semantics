package turksem.qamr

import cats.implicits._

import turksem._
import turksem.qamr._
import turksem.util._
import turksem.HasKeyIndices.ops._

import turkey._
import turkey.tasks._

import scala.util.Try
import upickle.default._

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text

trait PackagePlatformExtensions {

  // message definitions

  case object Pring

  case class ValidationResult[SID](
    prompt: GenerationPrompt[SID],
    sourceHITId: String,
    sourceAssignmentId: String,
    numValid: Int)

  case object SaveData

  // functions for chunking sentences into groups of content words

  def splitNum(n: Int): List[Int] =
    if(n <= 0) Nil
    else if(n <= 3) List(n)
    else if(n == 5) List(2, 3)
    else if(n == 6) List(3, 3)
    else if(n == 9) List(3, 3, 3)
    else 4 :: splitNum(n - 4)

  def splitList(l: List[Int]) = splitNum(l.size)
    .foldLeft((l, List.empty[List[Int]])) {
    case ((remaining, groups), groupSize) =>
      (remaining.drop(groupSize), remaining.take(groupSize) :: groups)
  }._2

  def tokenSplits(
    tokens: Vector[String])(
    implicit isStopword: IsStopword
  ) = splitList(tokens.indices.filter(i => !isStopword(tokens(i))).toList)

  // utility functions for constructing stats objects

  def emptyStatus[SID : HasKeyIndices](id: SID) = {
    SentenceStatus(
      id, id.keyIndices,
      Set.empty[Int], Set.empty[ValidationPrompt[SID]],
      List.empty[Assignment[List[ValidationAnswer]]])
  }

  // functions for writing and reading the data files

  // in the sentence index, each line is:
  // <id>\t<sentence>
  // where <id> is the string repr of a sentence ID
  // where <sentence> is a space-separated list of tokens in the sentence

  def makeSentenceIndex[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String
  ): String = {
    ids.map(id => s"${writeId(id)}\t${id.tokens.mkString(" ")}").mkString("\n")
  }

  // in the QA Pair index, each line is the following tab-separated fields:
  // 0: id -- string repr of sentence ID
  // 1: keywords -- space-separated list of integer indices of target tokens in the sentence in the HIT where the QA pair was written
  // 2: generator ID -- anonymized worker ID (string) of the worker who wrote the QA pair
  // 3: question index -- index of the question in the HIT in which it was generated
  // 4: keyword -- index (nat) of the keyword that elicited the QA pair
  // 5: question -- the question (string) written by the worker
  // 6: original answer -- space-separated list of indices (nat) of answer span given by original worker
  // 7, 9: validator worker IDs -- anonymized worker IDs (string) of validators who validated the answers
  // 8, 10: validator responses -- responses (see response format below) from respective validators

  // validator response format. each is one of the following:
  // - Invalid -- validator marked invalid
  // - Redundant: <index> -- where <index> is the integer index of the question in the same HIT this was redundant with
  // - <indices> -- space-separated list of indices (nat) of validator highlighted answer

  // to find target of redundancy, choose the one in the same chunk that has the indicated question index.

  // TODO make it take data structures representing already-aligned gen/val infos

  // TODO consolidate these functions into one place

  def makeReadableQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[GenerationPrompt[SID], List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]],
    keepQA: (SID, WordedQAPair, List[ValidationAnswer]) => Boolean = (
      (_: SID, _: WordedQAPair, _: List[ValidationAnswer]) => true)
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false
      sentenceSB.append(s"${idString}\t${nlpdata.util.Text.render(sentenceTokens)}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.keywords.min)) {
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
              sentenceSB.append(wqa.question + "\t") // question string written by worker
              sentenceSB.append(
                ((Answer(wqa.answer)) :: valResponses.map(_._2)).map { valAnswer =>
                  ValidationAnswer.render(sentenceTokens, valAnswer, genAssignment.response)
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

  def makeQAPairTSV[SID](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[GenerationPrompt[SID], List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]]
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.keywords.min)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))

            sb.append(idString + "\t") // 0: string representation of sentence ID
            sb.append(genHIT.prompt.keywords.mkString(" ") + "\t") // 1: space-separated set of keywords presented to turker
            sb.append(anonymizeWorker(genAssignment.workerId) + "\t") // 2: anonymized worker ID
            sb.append(qaIndex + "\t") // 3: index of the QA in the generation HIT
            sb.append(wqa.wordIndex + "\t") // 4: index of keyword in sentence
            sb.append(wqa.question + "\t") // 5: question string written by worker
            sb.append(wqa.answer.toVector.sorted.mkString(" ") + "\t") // 6: answer indices given by original worker
            if(valResponses.size != 2) {
              System.err.println("Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size)
            }
            sb.append(
              valResponses.map { case (valWorkerId, valAnswer) =>
                anonymizeWorker(valWorkerId) + "\t" + ValidationAnswer.renderIndices(valAnswer) // 7--10: validator responses
              }.mkString("\t")
            )
            sb.append("\n")
          }
        }
      }
    }
    sb.toString
  }

  def readTSV[SID](
    lines: Iterator[String],
    idFromString: String => SID
  ): QAData[SID] = {
    def readIntSet(s: String) = s.split(" ").map(_.toInt).toSet
    val sourcedQAs = lines.map { line =>
      val fields = line.split("\t")
      val id = idFromString(fields(0))
      val keywords = readIntSet(fields(1)).toList.sorted
      val generatorId = fields(2)
      val qaIndex = fields(3).toInt
      val qaPairId = QAPairId(id, keywords, generatorId, qaIndex)

      val keyword = fields(4).toInt
      val question = fields(5)
      val origAnswer = readIntSet(fields(6))
      val wqa = WordedQAPair(keyword, question, origAnswer)

      val valResponses = (7 until fields.size by 2).map { i =>
        (fields(i), ValidationAnswer.readIndices(fields(i + 1)))
      }.toList

      SourcedQA(qaPairId, wqa, valResponses)
    }.toList
    QAData(sourcedQAs)
  }

  // NOTE: used for final output of data
  def renderValidationAnswerFinal(
    va: ValidationAnswer
  ): String = va match {
    case InvalidQuestion => "Invalid"
    case Redundant(i) => s"Redundant-$i"
    case Answer(span) => span.toVector.sorted.mkString(" ")
  }

  def makeFinalQAPairTSV[SID](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[GenerationPrompt[SID], List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]],
    filterBadQAs: Boolean
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    val sb = new StringBuilder
    for(id <- ids) {
      var shouldIncludeSentence = false
      val sentenceSB = new StringBuilder
      val idString = writeId(id)
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.keywords.min)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))
            if(valResponses.size != 2) {
              System.err.println(
                "Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size
              )
            } else if(
              !filterBadQAs || (
                valResponses.forall(_._2.isAnswer) && beginsWithWhSpace(wqa.question))
            ) {
              shouldIncludeSentence = true
              sentenceSB.append(idString + "\t") // 0: string representation of sentence ID
              sentenceSB.append(genHIT.prompt.keywords.toList.sorted.mkString(" ") + "\t") // 1: space-separated set of keywords presented to turker
              sentenceSB.append(anonymizeWorker(genAssignment.workerId) + "\t") // 2: anonymized worker ID
              sentenceSB.append(qaIndex + "\t") // 3: index of the QA in the generation HIT
              sentenceSB.append(wqa.wordIndex + "\t") // 4: index of keyword in sentence
              sentenceSB.append(wqa.question + "\t") // 5: question string written by worker
              sentenceSB.append(wqa.answer.toVector.sorted.mkString(" ") + "\t") // 6: answer indices given by original worker
              sentenceSB.append(
                valResponses.map { case (valWorkerId, valAnswer) =>
                  anonymizeWorker(valWorkerId) + ":" + renderValidationAnswerFinal(valAnswer) // 7-8: validator responses
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

  def makeFinalReadableQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[GenerationPrompt[SID], List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]],
    filterBadQAs: Boolean
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    val sb = new StringBuilder
    for(id <- ids) {
      var shouldIncludeSentence = false
      val sentenceSB = new StringBuilder
      val idString = writeId(id)
      sentenceSB.append("#" + idString + "\n")
      sentenceSB.append(Text.render(id) + "\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.keywords.min)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))
            if(valResponses.size != 2) {
              System.err.println(
                "Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size
              )
            } else if(beginsWithWhSpace(wqa.question)) {
              valResponses.map(_._2.getAnswer).sequence.foreach { valAnswers =>
                val answersString = valAnswers.map(a => Text.renderSpan(id, a.indices)).mkString("\t")
                shouldIncludeSentence = true
                sentenceSB.append(f"${wqa.question}%-50s")
                sentenceSB.append(answersString)
                sentenceSB.append("\n")
              }
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString + "\n")
      }
    }
    sb.toString
  }
}
