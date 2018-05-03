package example.tqa

import cats._
import cats.implicits._

import qasrl.crowd._
import turksem.util._

import spacro._
import spacro.tasks._

import nlpdata.structure.AlignedToken

import nlpdata.datasets.wiki1k.Wiki1kFileSystemService
import nlpdata.datasets.wiki1k.Wiki1kPath
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.VerbForm
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

  import qasrl.labeling._

  def saveAnnotationData[A](
    filename: String,
    ids: Vector[SentenceId],
    genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
    anonymizeWorker: String => String,
    labelMapper: QuestionLabelMapper[String, A],
    labelRenderer: A => String
  ) = {
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeQAPairTSV(
        ids.toList,
        SentenceId.toString,
        genInfos,
        valInfos,
        // anonymizeWorker, // TODO not outputting worker ID in dataset right now
        labelMapper,
        labelRenderer)
    )
  }

  def saveAnnotationDataReadable(
    filename: String,
    ids: Vector[SentenceId],
    genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
    anonymizeWorker: String => String
  ) = {
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeReadableQAPairTSV(
        ids.toList,
        SentenceId.toString,
        anonymizeWorker,
        genInfos,
        valInfos,
        (id: SentenceId, qa: VerbQA, responses: List[QASRLValidationAnswer]) => responses.forall(_.isAnswer))
    )
  }

  lazy val genInfos = experiment.allGenInfos
  lazy val valInfos = experiment.allValInfos
  lazy val workerAnonymizationMap: Map[String, String] = {
    val allGenWorkerIdsIter = for {
      HITInfo(_, assignments) <- genInfos.iterator
      a <- assignments
    } yield a.workerId

    val allValWorkerIdsIter = for {
      HITInfo(_, assignments) <- valInfos.iterator
      a <- assignments
    } yield a.workerId

    val allWorkerIds = (allGenWorkerIdsIter ++ allValWorkerIdsIter).toSet

    val rand = new Random(1543754734L)
    val randomOrderedWorkerIds = rand.shuffle(allWorkerIds.toVector)
    randomOrderedWorkerIds.zipWithIndex.map {
      case (workerId, index) => workerId -> index.toString
    }.toMap
  }

  lazy val dataExporter = new AnnotationDataExporter(experiment)
  lazy val dataset = dataExporter.dataset(SentenceId.toString(_), identity[String](_))

  lazy val trainIds = (tqaTrainIds ++ wikipediaTrainIds ++ wikinewsTrainIds).toSet
  lazy val devIds = (tqaDevIds ++ wikipediaDevIds ++ wikinewsDevIds).toSet
  lazy val testIds = (tqaTestIds ++ wikipediaTestIds ++ wikinewsTestIds).toSet

  lazy val trainIdStringsSet = trainIds.map(SentenceId.toString)
  lazy val devIdStringsSet = devIds.map(SentenceId.toString)
  lazy val testIdStringsSet = testIds.map(SentenceId.toString)

  // lazy val trainDataset = dataset.filterSentenceIds(trainIdStringsSet)
  // lazy val devDataset = dataset.filterSentenceIds(devIdStringsSet)
  // lazy val testDataset = dataset.filterSentenceIds(testIdStringsSet)

  // sampling sentences for human eval & densification on dev/test
  lazy val (
    tqaDevHumanEvalIds, tqaTestHumanEvalIds,
    wikipediaDevHumanEvalIds, wikipediaTestHumanEvalIds,
    wikinewsDevHumanEvalIds, wikinewsTestHumanEvalIds
  ) = {
    val rand = new Random(1543754734L)

    val tqaDev = rand.shuffle(
      tqaDevIds.groupBy(_.topicId).toVector
    ).flatMap(_._2).take(1000).toSet
    val tqaTest = rand.shuffle(
      tqaTestIds.groupBy(_.topicId).toVector
    ).flatMap(_._2).take(1000).toSet

    val wikipediaDev = rand.shuffle(
      wikipediaDevIds.groupBy(id => id.copy(path = id.path.copy(sentenceNum = 0)))
    ).flatMap(_._2).take(1000).toSet
    val wikipediaTest = rand.shuffle(
      wikipediaTestIds.groupBy(id => id.copy(path = id.path.copy(sentenceNum = 0)))
    ).flatMap(_._2).take(1000).toSet

    val wikinewsDev = rand.shuffle(
      wikinewsDevIds.groupBy(id => id.copy(path = id.path.copy(sentenceNum = 0)))
    ).flatMap(_._2).take(1000).toSet
    val wikinewsTest = rand.shuffle(
      wikinewsTestIds.groupBy(id => id.copy(path = id.path.copy(sentenceNum = 0)))
    ).flatMap(_._2).take(1000).toSet

    (tqaDev, tqaTest, wikipediaDev, wikipediaTest, wikinewsDev, wikinewsTest)
  }

  def writeHumanEvalSentences(filename: String, ids: List[SentenceId]) = {
    saveOutputFile(
      s"human-sentences/$filename.tsv",
      ids.iterator.map(
        id => SentenceId.toString(id) + "\t" +
          id.tokens.mkString(" ") + "\t" +
          experiment.getKeyIndices(id).mkString(" ")
      ).mkString("\n")
    )
  }

  def writeAllHumanEvalSentences = {
    writeHumanEvalSentences("dev-tqa", tqaDevHumanEvalIds.toList.sorted)
    writeHumanEvalSentences("dev-wikipedia", wikipediaDevHumanEvalIds.toList.sorted)
    writeHumanEvalSentences("dev-wikinews", wikinewsDevHumanEvalIds.toList.sorted)
    writeHumanEvalSentences("test-tqa", tqaTestHumanEvalIds.toList.sorted)
    writeHumanEvalSentences("test-wikipedia", wikipediaTestHumanEvalIds.toList.sorted)
    writeHumanEvalSentences("test-wikinews", wikinewsTestHumanEvalIds.toList.sorted)
  }

  def writeAllSentences = {
    saveOutputFile(
      s"all-sentences/train.tsv",
      trainIds.iterator.map(
        id => SentenceId.toString(id) + "\t" +
          id.tokens.mkString(" ") + "\t" +
          experiment.getKeyIndices(id).mkString(" ")
      ).mkString("\n")
    )
    saveOutputFile(
      s"all-sentences/dev.tsv",
      devIds.iterator.map(
        id => SentenceId.toString(id) + "\t" +
          id.tokens.mkString(" ") + "\t" +
          experiment.getKeyIndices(id).mkString(" ")
      ).mkString("\n")
    )
    saveOutputFile(
      s"all-sentences/test.tsv",
      testIds.iterator.map(
        id => SentenceId.toString(id) + "\t" +
          id.tokens.mkString(" ") + "\t" +
          experiment.getKeyIndices(id).mkString(" ")
      ).mkString("\n")
    )
  }

  lazy val dataExporter = new AnnotationDataExporter(experiment)
  lazy val dataset = dataExporter.dataset(SentenceId.toString(_), identity[String](_))
  def writeDataset(part: String, dataset: QASRLDatasetNew.QASRLDataset) = {
    import io.circe.Printer
    import io.circe.syntax._
    import QASRLDatasetNew.QASRLDataset.JsonCodecs._
    saveOutputFile(
      s"$part/$label.jsonl",
      dataset.sentences.toVector.sortBy(_._1)
        .map(_._2.asJson)
        .map(Printer.noSpaces.pretty)
        .mkString("\n")
    )
  }

  // def writeDatasets = {
  //   import io.circe.Printer
  //   import io.circe.syntax._
  //   import QASRLDatasetNew.QASRLDataset.JsonCodecs._
  //   saveOutputFile(
  //     s"train/$label.jsonl",
  //     trainDataset.sentences.toVector.sortBy(_._1)
  //       .map(_._2.asJson)
  //       .map(Printer.noSpaces.pretty)
  //       .mkString("\n")
  //   )
  //   saveOutputFile(
  //     s"dev/$label.jsonl",
  //     devDataset.sentences.toVector.sortBy(_._1)
  //       .map(_._2.asJson)
  //       .map(Printer.noSpaces.pretty)
  //       .mkString("\n")
  //   )
  //   saveOutputFile(
  //     s"test/$label.jsonl",
  //     testDataset.sentences.toVector.sortBy(_._1)
  //       .map(_._2.asJson)
  //       .map(Printer.noSpaces.pretty)
  //       .mkString("\n")
  //   )
  // }

  def dataLabelIterator = for {
    hi <- genInfos.iterator
    verb <- hi.hit.prompt.id.tokens.lift(hi.hit.prompt.verbIndex).iterator
    verbInflectedForms <- inflections.getInflectedForms(verb.lowerCase).iterator
    a <- hi.assignments
  } yield (hi.hit.prompt.id.tokens, verbInflectedForms, a.response.map(_.question))

  // is good
  def checkDiscreteLabelingCompleteness = {
    dataLabelIterator.foreach { case (tokens, inflForms, questions) =>
      val simpleDiscrete = DiscreteLabel.getDiscreteLabels(tokens, inflForms, questions)
      (questions, simpleDiscrete).zipped.foreach { case (question, labelOpt) =>
        if(labelOpt.isEmpty) {
          System.err.println("Discrete label missing!")
          System.err.println(s"Question:  $question")
          System.err.println
        }
      }
    }
  }

  // is good
  def checkVerbTenseAbstraction = {
    dataLabelIterator.foreach { case (tokens, inflForms, questions) =>
      val origSlots = SlotBasedLabel.getSlotsForQuestion(tokens, inflForms, questions)
      val abstractedSlots = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(tokens, inflForms, questions)
      (questions, origSlots, abstractedSlots).zipped.foreach { case (question, orig, abst) =>
        val sameIfPresent = for {
          origVerb <- orig.map(_.verb)
          reifiedVerb <- abst.map(x => inflForms(x.verb))
        } yield reifiedVerb == origVerb
        if(!sameIfPresent.exists(identity)) {
          System.err.println("Difference between prev and cur tense when re-reified!")
          System.err.println(s"Question:  $question")
          System.err.println(s"Orig:      ${orig.foldMap(_.verb.toString)}")
          System.err.println(s"Reified:   ${abst.foldMap(x => inflForms(x.verb).toString)}")
          System.err.println
        }
      }
    }
  }

  // mainly just for reference, or if you need to re-produce all versions of the dataset
  def writeAllTSVCombinations = {
    writeAllTSVs[LowerCaseString]("string", QuestionLabelMapper.mapToLowerCase, _.toString)
    writeAllTSVs[DiscreteLabel]("discrete", DiscreteLabel.getDiscreteLabels, _.render)
    writeAllTSVs[SlotBasedLabel[LowerCaseString]](
      "slots", SlotBasedLabel.getSlotsForQuestion, _.renderWithSeparator(identity, ","))
    writeAllTSVs[SlotBasedLabel[VerbForm]](
      "slots-tense", SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion, _.renderWithSeparator(_.toString.lowerCase, ","))
    writeReadableTSVs
  }

  def writeAllTSVs[Label](
    labelType: String,
    labelMapper: QuestionLabelMapper[String, Label],
    labelRenderer: Label => String
  ) = {
    // train
    saveAnnotationData(s"$label/$labelType/train/tqa", tqaTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/train/wikipedia", wikipediaTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/train/wikinews", wikinewsTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    // dev
    saveAnnotationData(s"$label/$labelType/dev/tqa", tqaDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/dev/wikipedia", wikipediaDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/dev/wikinews", wikinewsDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    // test
    saveAnnotationData(s"$label/$labelType/test/tqa", tqaTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/test/wikipedia", wikipediaTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
    saveAnnotationData(s"$label/$labelType/test/wikinews", wikinewsTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  }

  def writeReadableTSVs = {
    // train
    saveAnnotationDataReadable(s"$label/readable/train/tqa", tqaTrainIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/train/wikipedia", wikipediaTrainIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/train/wikinews", wikinewsTrainIds, genInfos, valInfos, workerAnonymizationMap)
    // dev
    saveAnnotationDataReadable(s"$label/readable/dev/tqa", tqaDevIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/dev/wikipedia", wikipediaDevIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/dev/wikinews", wikinewsDevIds, genInfos, valInfos, workerAnonymizationMap)
    // test
    saveAnnotationDataReadable(s"$label/readable/test/tqa", tqaTestIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/test/wikipedia", wikipediaTestIds, genInfos, valInfos, workerAnonymizationMap)
    saveAnnotationDataReadable(s"$label/readable/test/wikinews", wikinewsTestIds, genInfos, valInfos, workerAnonymizationMap)
  }
}
