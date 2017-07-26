package turksem.qamr.annotation

import turksem._
import turksem.qamr._
import turksem.util._

import turkey._
import turkey.tasks._

import scala.util.Try
import upickle.default._

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

  def makeStats[SID : HasTokens : Reader](
    status: SentenceStatus[SID],
    genHITTypeId: String,
    valHITTypeId: String)(
    implicit config: TaskConfig,
    settings: PipelineSettings
  ): SentenceStats[SID] = {
    val allValidations = status.finishedAssignments
    val id = status.id
    val sentence = id.tokens
    val allValHITIds = allValidations.map(_.hitId).toSet
    val valHITInfos = allValHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[ValidationPrompt[SID], List[ValidationAnswer]](valHITTypeId, hitId).get)
    val allGenHITIds = valHITInfos.map(_.hit.prompt.sourceHITId).toSet
    val genHITInfos = allGenHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[GenerationPrompt[SID], List[WordedQAPair]](genHITTypeId, hitId).get)
    val sentenceHITInfo = SentenceHITInfo(sentence, genHITInfos, valHITInfos)

    val earliestTime = Try(
      genHITInfos.flatMap(_.assignments).map(_.acceptTime).min
    ).toOption.getOrElse(0L)
    val latestTime = Try(
      (valHITInfos.flatMap(_.assignments).map(_.submitTime) ++
         genHITInfos.flatMap(_.assignments).map(_.submitTime)).max
    ).toOption.getOrElse(0L)

    val alignedValidations = sentenceHITInfo.alignValidations
    val allKeywords = genHITInfos.map(_.hit.prompt.keywords).flatten.toSet
    val qaPairsEachKeywordPrompt = for {
      HITInfo(hit, assignments) <- genHITInfos
      assignment <- assignments
      keywordIndex <- hit.prompt.keywords.toList
    } yield assignment.response.filter(_.wordIndex == keywordIndex).size
    val qaPairsEachKeywordActual = for {
      keywordIndex <- allKeywords.toList
    } yield {
      val qaPairs = for {
        HITInfo(hit, assignments) <- genHITInfos
        assignment <- assignments
        wqa @ WordedQAPair(_, question, answerIndices) <- assignment.response
        if (answerIndices contains keywordIndex) || question.toLowerCase.contains(sentence(keywordIndex).toLowerCase)
      } yield wqa
      qaPairs.size
    }
    val validationLatencies = for {
      HITInfo(_, assignments) <- genHITInfos
      assignment <- assignments
      validations = for {
        HITInfo(valHIT, valAssignments) <- valHITInfos
        if valHIT.prompt.sourceAssignmentId == assignment.assignmentId
      } yield valAssignments.map(_.submitTime).max
      if !validations.isEmpty
      completion = validations.max
    } yield ((completion - assignment.submitTime) / 1000L).toInt  // seconds

    val numQAPairs = genHITInfos.flatMap(_.assignments).flatMap(_.response).size
    val numValidQAPairs = alignedValidations
      .map(av => ValidationAnswer.numValidQuestions(av.valAssignments.map(_.response)))
      .sum
    val completionTime = Try(
      valHITInfos.flatMap(_.assignments).map(_.submitTime).max
    ).toOption.getOrElse(0L)
    val genCost = alignedValidations.map(_.genCost).sum
    val valCost = alignedValidations.map(_.valCost).sum
    val genHITIds = genHITInfos.map(_.hit.hitId).toSet
    val valHITIds = valHITInfos.map(_.hit.hitId).toSet
    SentenceStats(
      id,
      earliestTime, latestTime,
      allKeywords.size,
      numQAPairs,
      numValidQAPairs,
      qaPairsEachKeywordPrompt,
      qaPairsEachKeywordActual,
      validationLatencies,
      completionTime,
      genCost, valCost,
      genHITIds, valHITIds)
  }

  def emptyStatus[SID : HasTokens](id: SID)(implicit isStopword: IsStopword) = {
    val sentence = id.tokens
    val allKeywords = sentence.indices
      .filter(i => !isStopword(sentence(i)))
      .toSet
    SentenceStatus(
      id, allKeywords,
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

  def makeReadableQAPairTSV[SID : HasTokens](
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
      val sentenceTokens = id.tokens
      sb.append(s"\t${idString}\t${nlpdata.util.Text.render(sentenceTokens)}\n")
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

            sb.append(genHIT.prompt.keywords.mkString(",") + "\t") // 1: space-separated set of keywords presented to turker
            sb.append(anonymizeWorker(genAssignment.workerId) + "\t") // 2: anonymized worker ID
            sb.append(wqa.question + "\t") // 5: question string written by worker
            sb.append(
              ((Answer(wqa.answer)) :: valResponses.map(_._2)).map { valAnswer =>
                ValidationAnswer.render(sentenceTokens, valAnswer, genAssignment.response)
              }.mkString("\t")
            )
            sb.append("\n")
          }
        }
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
}
