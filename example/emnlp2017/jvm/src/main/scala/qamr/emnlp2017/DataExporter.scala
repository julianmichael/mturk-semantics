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

  // TODO maybe move to annotation package in orig qamr project

  // def squadFormattedFileForWiki(excludedTitles: Set[String]): String = {
  //   // (validQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]])
  //   // data.sentenceToQAs
  //   import argonaut._
  //   import Argonaut._
  //   val idsByFile = data.sentenceToQAs.keys.collect {
  //     case id @ WikiSentenceId(wikiPath) => id
  //   }.groupBy(_.path.filePath).filter { case (filePath, _) =>
  //       val title = Wiki1k.getFile(filePath).get.title
  //       if(!excludedTitles.contains(title)) {
  //         true
  //       } else {
  //         System.out.println(s"Excluding file with title: $title")
  //         false
  //       }
  //   }

  //   def getAnswerSpanJson(tokens: Vector[String], answer: Set[Int]) = {
  //     val filledOutAnswer = (answer.min to answer.max).toSet
  //     val renderedSentence = Text.render(tokens)
  //     val (answerStart, answerText) = getOffsetAndSpan(tokens, filledOutAnswer)
  //     // stuff looked good (better, in fact, bc of treatment of quotes). if there are more problems, uncomment this and investigate.
  //     // val otherText = Text.renderSpan(tokens, filledOutAnswer).trim
  //     // if(!answerText.equals(otherText)) {
  //     //   System.err.println(
  //     //     s"Problem for sentence\n${Text.render(tokens)}\nExpected answer:\n$otherText \nPrinted answer:\n$answerText")
  //     // }
  //     Json.obj(
  //       "answer_start" -> jNumber(answerStart),
  //       "text" -> jString(answerText)
  //     )
  //   }

  //   def getQAJson(sentenceId: WikiSentenceId, sentenceTokens: Vector[String], qIndex: Int, question: String, answers: List[Set[Int]]) = {
  //     Json.obj(
  //       "answers" -> Json.array(answers.map(a => getAnswerSpanJson(sentenceTokens, a)): _*),
  //       "question" -> jString(question),
  //       "id" -> jString(s"${sentenceId.readableFileString}::${sentenceId.readableSentenceIndex}::$qIndex")
  //     )
  //   }

  //   def getSentenceJson(sentenceId: WikiSentenceId) = {
  //     val sentenceTokens = getTokensForId(sentenceId)
  //     val qas = data.sentenceToQAs(sentenceId).zipWithIndex.map {
  //       case (sqa, qIndex) => getQAJson(sentenceId, sentenceTokens, qIndex, sqa.question, sqa.answers)
  //     }.toSeq

  //     Json.obj(
  //       "context" -> jString(Text.render(getTokensForId(sentenceId))),
  //       "qas" -> Json.array(qas: _*)
  //     )
  //   }

  //   val files: Seq[Json] = idsByFile.keys.toSeq.map { filePath =>
  //     val wikiFile = Wiki1k.getFile(filePath).get
  //     val title = wikiFile.title
  //     val sentenceIds = idsByFile(filePath)
  //     val sentenceJsons = sentenceIds.map(getSentenceJson)
  //     Json.obj(
  //       "title" -> jString(title),
  //       "paragraphs" -> Json.array(sentenceJsons.toSeq: _*)
  //     )
  //   }

  //   val result = Json.obj(
  //     "data" -> Json.array(files: _*),
  //     "version" -> jString("1.1")
  //   )

  //   result.nospaces
  // }

  // def writeAllSquadFormatted(filename: String, excludedTitles: Set[String]) = {
  //   // val allIds = allGenInfos.map(_.hit.prompt.id).collect {
  //   //   case id @ WikiSentenceId(_) => id
  //   // }.toSet.toList
  //   System.out.println(s"Writing squad file $filename")
  //   saveDataFile(experimentName, filename, squadFormattedFileForWiki(excludedTitles))
  // }

}
