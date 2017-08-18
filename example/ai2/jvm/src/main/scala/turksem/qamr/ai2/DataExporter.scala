package turksem.qamr.ai2

import turksem.qamr._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.util.Text

/** Exports the data specific to our actual run for the EMNLP 2017 submission.
  * Requires that this data is stored in the HIT Data Service with the exact metadata
  * (hit type IDs hard-coded here) as in the original run.
  *
  * You shouldn't expect to be able to run this unless you have the original, non-anonymized data
  * gathered during the run on MTurk.
  */
class Ai2DataExporter(implicit config: TaskConfig) {

  val pipeline = new Ai2AnnotationSetup

  import config._
  import pipeline._

  val genHITTypeId = "3554GQY3BJXDVEL54N24OMP560NSLM"
  val valHITTypeId = "3LAWS9V4CCTFCTDV94LP13R03M38YF"

  lazy val allGenInfos: List[HITInfo[GenerationPrompt[Ai2SentenceId], List[WordedQAPair]]] =
    hitDataService.getAllHITInfo[GenerationPrompt[Ai2SentenceId], List[WordedQAPair]](genHITTypeId).get
  lazy val allValInfos: List[HITInfo[ValidationPrompt[Ai2SentenceId], List[ValidationAnswer]]] =
    hitDataService.getAllHITInfo[ValidationPrompt[Ai2SentenceId], List[ValidationAnswer]](valHITTypeId).get

  lazy val workerAnonymizationMap: Map[String, String] = {
    val allGenWorkerIdsIter = for {
      HITInfo(_, assignments) <- allGenInfos.iterator
      a <- assignments
    } yield a.workerId

    val allValWorkerIdsIter = for {
      HITInfo(_, assignments) <- allValInfos.iterator
      a <- assignments
    } yield a.workerId

    val allWorkerIds = (allGenWorkerIdsIter ++ allValWorkerIdsIter).toSet

    val rand = new scala.util.Random(1543754734L)
    val randomOrderedWorkerIds = rand.shuffle(allWorkerIds.toVector)
    randomOrderedWorkerIds.zipWithIndex.map {
      case (workerId, index) => workerId -> index.toString
    }.toMap
  }

  def makeReadableTSV(ids: List[Ai2SentenceId]): String =
    makeReadableQAPairTSV(ids, Ai2SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos)

  def writeReadableTSVs = {
    val kbSentenceIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ KBSentenceId(_, _) => id
    }.toSet.toList
    val mathProblemSentenceIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ MathProblemSentenceId(_, _) => id
    }.toSet.toList
    saveOutputFile("aristo-readable.tsv", makeReadableTSV(kbSentenceIds))
    saveOutputFile("euclid-readable.tsv", makeReadableTSV(mathProblemSentenceIds))
  }

  def makeTSV(ids: List[Ai2SentenceId]): String =
    makeQAPairTSV(ids, Ai2SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos)

  def writeTSVs = {
    val kbSentenceIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ KBSentenceId(_, _) => id
    }.toSet.toList
    val mathProblemSentenceIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ MathProblemSentenceId(_, _) => id
    }.toSet.toList
    saveOutputFile(
      "aristo-sentences.tsv",
      makeSentenceIndex(kbSentenceIds: List[Ai2SentenceId], Ai2SentenceId.toString))
    saveOutputFile(
      "euclid-sentences.tsv",
      makeSentenceIndex(mathProblemSentenceIds: List[Ai2SentenceId], Ai2SentenceId.toString))
    saveOutputFile("aristo-qas.tsv", makeTSV(kbSentenceIds))
    saveOutputFile("euclid-qas.tsv", makeTSV(mathProblemSentenceIds))
  }
}
