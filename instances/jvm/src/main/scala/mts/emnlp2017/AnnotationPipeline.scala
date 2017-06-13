package mts.emnlp2017

import mts._
import mts.annotation._
import mts.util._

import turkey._
import turkey.tasks._

import nlpdata.structure._
import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Random

import monocle._
import monocle.macros._

import upickle.default._

class PaperAnnotationPipeline(implicit config: TaskConfig) {

  import config._

  // saved these manually, see code in package.scala TODO where is it actually? lol
  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    loadDataFile(experimentName, "origQASRLPaths.txt").get.head
  )

  val numPTB = 150

  lazy val (ptbTrain, ptbDev, ptbTest) = {
    val shuffleRand = new Random(832592735L)
    val (train, devTestRest) = shuffleRand.shuffle(origQASRLPaths).splitAt(numPTB * 4 / 5)
    val (dev, testRest) = devTestRest.splitAt(numPTB / 10)
    val test = testRest.take(numPTB / 10)
    (train, dev, test)
  }

  lazy val ptb100ForAMR = PTB.allPTBSentencePaths.get.take(103).map(PTBSentenceId.apply).toList

  def getWikiSentences(rand: Random, filePaths: Vector[Wiki1kPath], numSentences: Int) = {
    rand.shuffle(
      filePaths.flatMap(p => Wiki1k.getFile(p).get.paragraphs)
    ).filter(p =>
      !p.exists(sentence =>
        sentence.tokens.exists(t =>
          Text.normalizeToken(t) == "\\"))
    ).flatten.map(s => s.path).take(numSentences)
  }

  val numWikipedia = 2500
  // val numWikipedia = 5

  lazy val (wikipediaTrain, wikipediaDev, wikipediaTest) = {
    val shuffleRand = new Random(1230976L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      Wiki1k.wiki1kPathsForDomain("wikipedia")
    ).splitAt(640)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikipedia * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikipedia / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikipedia / 10)
    (train, dev, test)
  }

  val numWikinews = 2500
  // val numWikinews = 5

  lazy val (wikinewsTrain, wikinewsDev, wikinewsTest) = {
    val shuffleRand = new Random(1246902L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      Wiki1k.wiki1kPathsForDomain("wikinews")
        .sortBy(-_.suffix.toInt) // relies on wikinews IDs being ints... true as of now
        .take(1000)
    ).splitAt(800)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikinews * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikinews / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikinews / 10)
    (train, dev, test)
  }

  lazy val trainIds = ptbTrain.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTrain.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTrain.map(WikiSentenceId(_): SentenceId)
  lazy val trainIDSet = trainIds.toSet
  def isTrain(id: SentenceId) = trainIDSet.contains(id)

  lazy val devIds = ptbDev.map(PTBSentenceId(_): SentenceId) ++
    wikipediaDev.map(WikiSentenceId(_): SentenceId) ++
    wikinewsDev.map(WikiSentenceId(_): SentenceId)
  lazy val devIDSet = devIds.toSet
  def isDev(id: SentenceId) = devIDSet.contains(id)

  lazy val testIds = ptbTest.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTest.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTest.map(WikiSentenceId(_): SentenceId)
  lazy val testIDSet = testIds.toSet
  def isTest(id: SentenceId) = testIDSet.contains(id)

  lazy val sourceIds = {
    val idShuffleRand = new Random(218469L)
    idShuffleRand.shuffle(trainIds ++ devIds ++ testIds)
      .filter {
      case WikiSentenceId(path) =>
        !path.filePath.suffix.contains("785582") && // this is apparently a FRENCH INTERVIEW
          !path.filePath.suffix.contains("648587") // this is apparently a SPANISH ARTICLE
      case _ => true
    }
  }

  lazy val allIds = (ptb100ForAMR ++ sourceIds).toVector

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  implicit lazy val isStopword = IsStopword(isReallyUninteresting)

  def numGenerationAssignmentsForPrompt(p: GenerationPrompt[SentenceId]) = p.id match {
    case PTBSentenceId(_) => 5
    case id @ WikiSentenceId(_) => if(isTrain(id)) 1 else 3
  }

  lazy val experiment = new AnnotationPipeline(allIds, numGenerationAssignmentsForPrompt)

  def startAnnotationPipeline = experiment

}

class PaperAnnotationData(
  implicit config: TaskConfig
) {

  val pipeline = new PaperAnnotationPipeline

  import config._
  import pipeline._

  // TODO put this in analysis
  // to use the below, need to have the actual data we collected downloaded in its original format

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

  def makeTSV(
    ids: List[SentenceId],
    genInfos: List[HITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]]]
  ): String = {
    import scalaz._
    import Scalaz._
    import scala.language.higherKinds
    type PrintingState[A] = State[List[String], A]
    type Printing[A] = ListT[PrintingState, A]
    def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
    def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))
    val processor = for {
      id <- iter(ids)
      sentence = getTokensForId(id)
      _ <- append("\t\t\t" + sentence.mkString(" ") + "\n")
      (genWorkerId, keywords, WordedQAPair(keywordIndex, question, answerIndices), valAnswers, valAnswersString, valFeedback) <- iter {
        val qaPairs = for {
          HITInfo(genHIT, genAssignments) <- genInfos
          if genHIT.prompt.id == id
          genAssignment <- genAssignments
          chosenValInfo <- valInfos.find(_.hit.prompt.sourceAssignmentId.equals(genAssignment.assignmentId)).toList
          (wqa, qaIndex) <- genAssignment.response.zipWithIndex
          valAnswers = chosenValInfo.assignments.map(a => a.response(qaIndex))
          valFeedback = chosenValInfo.assignments.map(a => a.feedback).filterNot(_.isEmpty)
        } yield (
          genAssignment.workerId,
          genHIT.prompt.keywords,
          wqa,
          valAnswers,
          valAnswers.map(valAnswer =>
            renderValidationAnswer(sentence, valAnswer, genAssignment.response)
          ).mkString("\t"),
          valFeedback.mkString("\t")
        )
        qaPairs.sortBy(_._3.wordIndex)
      }
      questionTokens = tokenize(question).toVector
      questionSentenceAlignments = getQuestionSentenceAlignments(sentence, questionTokens) // q-s
      qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
      _ <- append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${keywords.toVector.sorted.mkString(" ")}\t${genWorkerId}\t")
      _ <- append(s"${sentence(keywordIndex)} ($keywordIndex)\t$qsAlignmentsString\t${questionTokens.mkString(" ")}\t${valAnswers.size + 1}\t")
      _ <- append(Text.renderSpan(sentence, answerIndices) + s"\t$valAnswersString\t")
      _ <- append(s"${answerIndices.mkString(" ")}\t")
      _ <- append(valAnswers.map(_.getAnswer.map(_.indices.mkString(" ")).getOrElse("")).mkString("\t"))
      _ <- append(s"\t$valFeedback")
      _ <- append("\n")
    } yield ()
    processor.run.exec(Nil).reverse.mkString
  }

  def writeTSVs(
    genInfos: List[HITInfo[GenerationPrompt[SentenceId], List[WordedQAPair]]] = allGenInfos,
    valInfos: List[HITInfo[ValidationPrompt[SentenceId], List[ValidationAnswer]]] = allValInfos
  ) = {
    val allIds = genInfos.map(_.hit.prompt.id).collect {
      case id @ WikiSentenceId(_) => id
    }.toSet.toList
    val trainIds = allIds.filter(isTrain)
    val devIds = allIds.filter(isDev)
    val testIds = allIds.filter(isTest)
    saveDataFile(experimentName, "train.tsv", makeTSV(trainIds, genInfos, valInfos))
    saveDataFile(experimentName, "dev.tsv", makeTSV(devIds, genInfos, valInfos))
    saveDataFile(experimentName, "test.tsv", makeTSV(testIds, genInfos, valInfos))
  }

  def writePTBTSVs = {
    val genInfos = allGenInfos
    val valInfos = allValInfos
    val trainIds = ptbTrain.map(PTBSentenceId.apply).toList
    val devIds = ptbDev.map(PTBSentenceId.apply).toList
    val testIds = ptbTest.map(PTBSentenceId.apply).toList
    val amrIds = ptb100ForAMR.toList
    saveDataFile(experimentName, "ptb-train.tsv", makeTSV(trainIds, genInfos, valInfos))
    saveDataFile(experimentName, "ptb-dev.tsv", makeTSV(devIds, genInfos, valInfos))
    saveDataFile(experimentName, "ptb-test.tsv", makeTSV(testIds, genInfos, valInfos))
    saveDataFile(experimentName, "ptb-amr.tsv", makeTSV(amrIds, genInfos, valInfos))
  }

}
