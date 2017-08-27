package example.emnlp2017

import turksem.qamr._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.util.Text
import nlpdata.util.HasTokens.ops._

/** Exports the data specific to our actual run for the EMNLP 2017 submission.
  * Requires that this data is stored in the HIT Data Service with the exact metadata
  * (hit type IDs hard-coded here) as in the original run.
  *
  * You shouldn't expect to be able to run this unless you have the original, non-anonymized data
  * gathered during the run on MTurk.
  */
class DataExporter(implicit config: TaskConfig) {

  val setup = new AnnotationSetup

  import config._
  import setup._

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

  lazy val allIds = allGenInfos.map(_.hit.prompt.id).collect {
    case id @ WikiSentenceId(_) => id
  }.toSet.toList
  lazy val trainIds = allIds.filter(isTrain)
  lazy val devIds = allIds.filter(isDev)
  lazy val testIds = allIds.filter(isTest)

  lazy val ptbTrainIds = ptbTrain.map(PTBSentenceId.apply).toList
  lazy val ptbDevIds = ptbDev.map(PTBSentenceId.apply).toList
  lazy val ptbTestIds = ptbTest.map(PTBSentenceId.apply).toList
  lazy val ptbAMRIds = ptb100ForAMR.toList
  lazy val allPTBIds = ptbTrainIds ++ ptbDevIds ++ ptbTestIds ++ ptbAMRIds

  def isQAGood(sid: SentenceId, wqa: WordedQAPair, valAnswers: List[ValidationAnswer]) =
    valAnswers.forall(_.isAnswer)

  def makeCompleteTSV(ids: List[SentenceId]): String =
    makeQAPairTSV(ids, SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos)

  def makeReadableTSV(
    ids: List[SentenceId],
    includeQA: (SentenceId, WordedQAPair, List[ValidationAnswer]) => Boolean = isQAGood): String =
    makeReadableQAPairTSV(ids, SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos, includeQA)

  def writeTSVs = {
    saveOutputFile("sentences.tsv", makeSentenceIndex(allIds: List[SentenceId], SentenceId.toString))
    saveOutputFile("train.tsv", makeCompleteTSV(trainIds))
    saveOutputFile("dev.tsv", makeCompleteTSV(devIds))
    saveOutputFile("test.tsv", makeCompleteTSV(testIds))
    saveOutputFile("train-readable.tsv", makeReadableTSV(trainIds))
    saveOutputFile("dev-readable.tsv", makeReadableTSV(devIds))
    saveOutputFile("test-readable.tsv", makeReadableTSV(testIds))
  }

  def writePTBTSVs = {
    saveOutputFile("ptb-sentences.tsv", makeSentenceIndex(allPTBIds, SentenceId.toString))
    saveOutputFile("ptb-train.tsv", makeCompleteTSV(ptbTrainIds))
    saveOutputFile("ptb-dev.tsv", makeCompleteTSV(ptbDevIds))
    saveOutputFile("ptb-test.tsv", makeCompleteTSV(ptbTestIds))
    saveOutputFile("ptb-amr.tsv", makeCompleteTSV(ptbAMRIds))
    saveOutputFile("ptb-readable.tsv", makeReadableTSV(allPTBIds))
  }

  def goodQAContainsOneSentenceWord(sid: SentenceId, wqa: WordedQAPair, valAnswers: List[ValidationAnswer]) = {
    val questionTokens = Tokenizer.tokenize(wqa.question)
    isQAGood(sid, wqa, valAnswers) && getWordsInQuestion(sid.tokens, questionTokens).size == 1
  }

  // TODO write TSVs for more subsets of questions, as well as interesting templates.
  // in particular:
  // 2. templates for questions using only a single word from the sentence.
  // 3. templates for such questions, split by POS of the word, and then listed for each POS grouped together.
  // 4. the particular case of questions about nouns where the answer is also a single noun
  def writeFilteredTSVs = {
    // only one word:
    // TODO how many questions fit this criterion?
    saveOutputFile("train-1word.tsv", makeReadableTSV(trainIds, goodQAContainsOneSentenceWord))
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
