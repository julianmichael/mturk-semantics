package qamr.emnlp2017

import qamr._
import qamr.annotation._
import qamr.util._

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
class DataExporter(implicit config: TaskConfig) {

  val pipeline = new AnnotationSetup

  import config._
  import pipeline._

  val oldGenHITTypeId = "3X8M0CO8US8JERH7QA0GGQIWAEHPVL"
  val oldValHITTypeId = "3OR5EJIUG2QY9PC04VUEYEGYR3Q9UL"

  val oldGenHITTypeId2 = "36SUH4ZPJUVEFKCRRRIAVB1LGOZ705"
  val oldValHITTypeId2 = "3XRW87W7OXAP1BEXLSFQFDKLIPNTQ0"

  val oldGenHITTypeId3 = "3554GQY3BJXDVEL54N24OMP560NSLM"
  val oldValHITTypeId3 = "3AYVNGH59IZRTO9MQCW5NV51ECHYDQ"

  lazy val allGenInfos: List[HITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]]] =
    (hitDataService.getAllHITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]](oldGenHITTypeId).get
       ++ hitDataService.getAllHITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]](oldGenHITTypeId2).get
       ++ hitDataService.getAllHITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]](oldGenHITTypeId3).get)
  lazy val allValInfos: List[HITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]]] =
    (hitDataService.getAllHITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]](oldValHITTypeId).get
       ++ hitDataService.getAllHITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]](oldValHITTypeId2).get
       ++ hitDataService.getAllHITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]](oldValHITTypeId3).get)

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

  def makeTSV(ids: List[SentenceId]): String =
    makeQAPairTSV(ids, SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos)

  // TODO execute this and actually make sure it's all written correctly

  def writeTSVs = {
    val allIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ WikiSentenceId(_) => id
    }.toSet.toList
    val trainIds = allIds.filter(isTrain)
    val devIds = allIds.filter(isDev)
    val testIds = allIds.filter(isTest)
    saveOutputFile("sentences.tsv", makeSentenceIndex(allIds: List[SentenceId], SentenceId.toString))
    saveOutputFile("train.tsv", makeTSV(trainIds))
    saveOutputFile("dev.tsv", makeTSV(devIds))
    saveOutputFile("test.tsv", makeTSV(testIds))
  }

  def writePTBTSVs = {
    val trainIds = ptbTrain.map(PTBSentenceId.apply).toList
    val devIds = ptbDev.map(PTBSentenceId.apply).toList
    val testIds = ptbTest.map(PTBSentenceId.apply).toList
    val amrIds = ptb100ForAMR.toList
    val allPTBIds = trainIds ++ devIds ++ testIds ++ amrIds
    saveOutputFile("ptb-sentences.tsv", makeSentenceIndex(allPTBIds, SentenceId.toString))
    saveOutputFile("ptb-train.tsv", makeTSV(trainIds))
    saveOutputFile("ptb-dev.tsv", makeTSV(devIds))
    saveOutputFile("ptb-test.tsv", makeTSV(testIds))
    saveOutputFile("ptb-amr.tsv", makeTSV(amrIds))
  }

}
