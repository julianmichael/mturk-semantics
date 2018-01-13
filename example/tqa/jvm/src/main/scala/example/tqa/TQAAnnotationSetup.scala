package example.tqa

import cats._
import cats.implicits._

import qasrl.crowd._
import turksem.qasrl.QALabelMapper
import turksem.util._

import spacro._
import spacro.tasks._

import nlpdata.structure.AlignedToken

import nlpdata.datasets.wiki1k.Wiki1kFileSystemService
import nlpdata.datasets.wiki1k.Wiki1kPath
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.tqa.TQAFileSystemService

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Try

import upickle.default._

import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import scala.util.Try
import scala.util.Random

import upickle.default._

class TQAAnnotationSetup(
  val label: String,
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None)(
  implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = Paths.get(s"data/tqa/$label/live")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get(s"data/tqa/$label/static")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    val directory = path.getParent
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("in").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  lazy val tqaTexts = new TQAFileSystemTopicTextService(
    resourcePath.resolve("tqa/tqa_sentences_trimmed.json")
  ).topicTexts

  lazy val tqaTrain = new TQAFileSystemService(
    resourcePath.resolve("tqa/train/tqa_v1_train.json")
  ).getDataset.get
  lazy val tqaDev = new TQAFileSystemService(
    resourcePath.resolve("tqa/val/tqa_v1_val.json")
  ).getDataset.get

  lazy val tqaTrainTopicIds = tqaTrain.topics.keySet
  lazy val tqaDevTopicIds = tqaDev.topics.keySet

  def isTQATrain(sid: TQASentenceId) = tqaTrainTopicIds.contains(sid.topicId)
  def isTQADev(sid: TQASentenceId) = tqaDevTopicIds.contains(sid.topicId)
  def isTQATest(sid: TQASentenceId) = !isTQATrain(sid) && !isTQADev(sid)

  lazy val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  val Wiki1k = new Wiki1kFileSystemService(
    resourcePath.resolve("wiki1k")
  )

  def getWikiSentences(rand: Random, filePaths: Vector[Wiki1kPath], numSentences: Int) = {
    rand.shuffle(
      filePaths.flatMap(p => Wiki1k.getFile(p).get.paragraphs)
    ).filter(p =>
      !p.exists(sentence =>
        sentence.tokens.exists(t =>
          Text.normalizeToken(t) == "\\"))
    ).flatten.map(s => WikiSentenceId(s.path)).take(numSentences)
  }

  val numWikipedia = 15000

  lazy val (wikipediaTrainIds, wikipediaDevIds, wikipediaTestIds) = {
    // reproduce train/dev/test split from QAMR
    val shuffleRand = new Random(1230976L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      Wiki1k.wiki1kPathsForDomain("wikipedia")
    ).splitAt(640)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    // use a different random seed so we get different sentences than QAMR
    val sentenceShuffleRand = new Random(89561915L)
    val train = getWikiSentences(sentenceShuffleRand, trainFiles, numWikipedia * 4 / 5)
    val dev = getWikiSentences(sentenceShuffleRand, devFiles, numWikipedia / 10)
    val test = getWikiSentences(sentenceShuffleRand, testFiles, numWikipedia / 10)
    (train, dev, test)
  }

  val numWikinews = 15000

  lazy val (wikinewsTrainIds, wikinewsDevIds, wikinewsTestIds) = {
    // reproduce train/dev/test split from QAMR
    val shuffleRand = new Random(1246902L)
    val (trainFiles, devFiles, testFiles) = {
      val (trainz, devTestRestFiles) = shuffleRand.shuffle(
        Wiki1k.wiki1kPathsForDomain("wikinews")
          .sortBy(-_.suffix.toInt) // relies on wikinews IDs being ints... true as of now
          .take(1000) // remove 1k most recent b/c not as well audited / lower quality
      ).splitAt(800)
      val (devz, testRestFiles) = devTestRestFiles.splitAt(80)
      val testz = testRestFiles.take(80)
      // filtering that we know to do before sentence selection.
      // so as not to throw off sentence selection,
      // any subsequent filtering (ie discovered during annotation) should be done later.
      def filterFiles(paths: Vector[Wiki1kPath]) =
        paths.filterNot(path =>
          path.suffix.contains("785582") || // this is apparently a French interview
            path.suffix.contains("648587")) // this is apparently a Spanish article
      (filterFiles(trainz), filterFiles(devz), filterFiles(testz))
    }

    // use a different random seed so we get different sentences than QAMR
    val sentenceShuffleRand = new Random(32186519L)
    val train = getWikiSentences(sentenceShuffleRand, trainFiles, numWikinews * 4 / 5)
    val dev = getWikiSentences(sentenceShuffleRand, devFiles, numWikinews / 10)
    val test = getWikiSentences(sentenceShuffleRand, testFiles, numWikinews / 10)
    (train, dev, test)
  }

  implicit object SentenceIdHasAlignedTokens extends HasTokens[SentenceId] {
    override def getTokens(id: SentenceId): Vector[String] = id match {
      case TQASentenceId(topicId, sentenceIndex) => tqaTexts(topicId).sentences(sentenceIndex).map(_.token)
      case WikiSentenceId(path) => Wiki1k.getSentence(path).get.tokens
    }
  }

  import scala.annotation.tailrec
  // @tailrec private def weightedRoundRobinAux[A](soFar: Vector[A], vectors: List[Vector[A]]): Vector[A] = {
  //     if(vectors.isEmpty) soFar else { // hit base case because filter out empties
  //       val smallestSize = vectors.map(_.size).min // works bc nonempty
  //       val (processedRemains, newSoFar) = vectors.foldLeft((List.empty[Vector[A]], soFar)) {
  //         case ((remains, fullSoFar), vector) =>
  //           val sizeMultiplier = vector.size / smallestSize
  //           (vector.drop(sizeMultiplier) :: remains, fullSoFar ++ vector.take(sizeMultiplier))
  //       }
  //       weightedRoundRobinAux(newSoFar, processedRemains.reverse.filter(_.nonEmpty))
  //     }
  //   }
  // def weightedRoundRobin[A](vectors: List[Vector[A]]) = weightedRoundRobinAux(Vector.empty[A], vectors)

  @tailrec private def weightedRoundRobinRandomizedAux[A](
    soFar: Vector[A],
    vectors: List[Vector[A]],
    rand: Random
  ): Vector[A] = {
      if(vectors.isEmpty) soFar else { // hit base case because filter out empties
        val smallestSize = vectors.map(_.size).min // works bc nonempty
        val (processedRemains, newSoFar) = vectors.foldLeft((List.empty[Vector[A]], Vector.empty[A])) {
          case ((remains, soFarAcc), vector) =>
            val sizeMultiplier = vector.size / smallestSize
            (vector.drop(sizeMultiplier) :: remains, soFarAcc ++ vector.take(sizeMultiplier))
        }
        weightedRoundRobinRandomizedAux(soFar ++ rand.shuffle(newSoFar), processedRemains.reverse.filter(_.nonEmpty), rand)
      }
    }
  def weightedRoundRobinRandomized[A](
    vectors: List[Vector[A]],
    rand: Random
  ) = weightedRoundRobinRandomizedAux(Vector.empty[A], vectors, rand)

  // reorders a vector into a result s.t. prefixes of the result are
  // (roughly) maximally evenly distributed over the original vector
  private def evenDistributionAux[A](vector: Vector[A]): Iterator[A] = {
    if(vector.size <= 3) {
      vector.iterator
    } else {
      val (firstHalf, secondHalf) = vector.splitAt(vector.size / 2)
      evenDistributionAux(firstHalf).zip(evenDistributionAux(secondHalf)).flatMap {
        case (x, y) => x :: y :: Nil
      }
    }
  }
  def evenDistribution[A](vector: Vector[A]) = evenDistributionAux(vector).toVector

  lazy val (tqaTrainIds, tqaDevIds, tqaTestIds) = {
    val unshuffled = for {
      (topicId, topicText) <- tqaTexts
      index <- topicText.sentences.indices
    } yield TQASentenceId(topicId, index)
    val shuffleRand = new Random(2158369L)
    val shuffled = shuffleRand.shuffle(unshuffled.toVector)
    val train = shuffled.filter(isTQATrain)
    val dev = shuffled.filter(isTQADev)
    val test = shuffled.filter(isTQATest)
    (train, dev, test)
  }

  lazy val allIds = weightedRoundRobinRandomized(
    List(
      tqaTrainIds, tqaDevIds, tqaTestIds,
      wikipediaTrainIds, wikipediaDevIds, wikipediaTestIds,
      wikinewsTrainIds, wikinewsDevIds, wikinewsTestIds),
    new Random(5614129L))

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  def numGenerationAssignmentsForPrompt(p: QASRLGenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    allIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    frozenGenerationHITTypeId = frozenGenerationHITTypeId,
    frozenValidationHITTypeId = frozenValidationHITTypeId,
    generationAccuracyDisqualTypeLabel = None,
    generationCoverageDisqualTypeLabel = None,
    validationAgreementDisqualTypeLabel = None)

  def saveAnnotationData(
    filename: String,
    ids: Vector[SentenceId],
    genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
    questionLabelGetter: DataIO.QuestionLabelGetter = QALabelMapper.useQuestionString
  ) = {
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeQAPairTSV(
        ids.toList,
        SentenceId.toString,
        genInfos,
        valInfos,
        questionLabelGetter)
    )
  }

  def saveAnnotationDataReadable(
    filename: String,
    ids: Vector[SentenceId],
    genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]]
  ) = {
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeReadableQAPairTSV(
        ids.toList,
        SentenceId.toString,
        identity,
        genInfos,
        valInfos,
        (id: SentenceId, qa: VerbQA, responses: List[QASRLValidationAnswer]) => responses.forall(_.isAnswer))
    )
  }

  lazy val genInfos = experiment.allGenInfos
  lazy val valInfos = experiment.allValInfos

  // mainly just for reference, or if you need to re-produce all versions of the dataset
  def writeAllTSVCombinations = {
    writeAllTSVs("string", QALabelMapper.useQuestionString)
    writeAllTSVs("collapsed", QALabelMapper.getCollapsedLabels)
    writeAllTSVs("slots", QALabelMapper.getExplicitTemplateLabelsForQuestion)
    writeReadableTSVs
  }

  def writeAllTSVs(
    labelType: String,
    questionLabelGetter: DataIO.QuestionLabelGetter
  ) = {
    // train
    saveAnnotationData(s"$label/$labelType/train/tqa", tqaTrainIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/train/wikipedia", wikipediaTrainIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/train/wikinews", wikinewsTrainIds, genInfos, valInfos, questionLabelGetter)
    // dev
    saveAnnotationData(s"$label/$labelType/dev/tqa", tqaDevIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/dev/wikipedia", wikipediaDevIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/dev/wikinews", wikinewsDevIds, genInfos, valInfos, questionLabelGetter)
    // test
    saveAnnotationData(s"$label/$labelType/test/tqa", tqaTestIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/test/wikipedia", wikipediaTestIds, genInfos, valInfos, questionLabelGetter)
    saveAnnotationData(s"$label/$labelType/test/wikinews", wikinewsTestIds, genInfos, valInfos, questionLabelGetter)
  }

  def writeReadableTSVs = {
    // train
    saveAnnotationDataReadable(s"$label/readable/train/tqa", tqaTrainIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/train/wikipedia", wikipediaTrainIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/train/wikinews", wikinewsTrainIds, genInfos, valInfos)
    // dev
    saveAnnotationDataReadable(s"$label/readable/dev/tqa", tqaDevIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/dev/wikipedia", wikipediaDevIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/dev/wikinews", wikinewsDevIds, genInfos, valInfos)
    // test
    saveAnnotationDataReadable(s"$label/readable/test/tqa", tqaTestIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/test/wikipedia", wikipediaTestIds, genInfos, valInfos)
    saveAnnotationDataReadable(s"$label/readable/test/wikinews", wikinewsTestIds, genInfos, valInfos)
  }
}
