package example.tqa

import turksem.util._

import cats._
import cats.implicits._

import qasrl.crowd._
import qasrl.labeling._

import spacro._
import spacro.util.Span
import spacro.tasks._

import nlpdata.structure.AlignedToken

import nlpdata.datasets.wiki1k.Wiki1kFileSystemService
import nlpdata.datasets.wiki1k.Wiki1kPath
import nlpdata.datasets.wiktionary._
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

class TQAEvaluationSetup(
  val label: String,
  frozenEvaluationHITTypeId: Option[String] = None)(
  implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")
  val liveDataPath = Paths.get(s"data/tqaeval/$label/live")
  val staticDataPath = Paths.get(s"data/tqaeval/$label/static")

  implicit val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

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

  lazy val Wiktionary = new WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  val Wiki1k = new Wiki1kFileSystemService(
    resourcePath.resolve("wiki1k")
  )

  lazy val tqaTexts = new TQAFileSystemTopicTextService(
    resourcePath.resolve("tqa/tqa_sentences_trimmed.json")
  ).topicTexts

  def readVerbForm(lcs: LowerCaseString): VerbForm = lcs.toString match {
    case "stem" => Stem
    case "presentsingular3rd" => PresentSingular3rd
    case "presentparticiple" => PresentParticiple
    case "past" => Past
    case "pastparticiple" => PastParticiple
    case _ => ???
  }

  def allTokensIter = {
    val trainTokensIter = loadInputFile("test_expansion_validations.tsv").get.iterator
      .filter(l => !l.startsWith("\t"))
      .flatMap(_.split("\t").last.split(" ").iterator)
    val devTokensIter = loadInputFile("test_dev_validations.tsv").get.iterator
      .filter(l => !l.startsWith("\t"))
      .flatMap(_.split("\t").last.split(" ").iterator)
    trainTokensIter ++ devTokensIter
  }

  implicit lazy val inflections = Wiktionary.getInflectionsForTokens(allTokensIter)

  def readPromptsFromFile(filename: String, source: String): Vector[QASRLEvaluationPrompt[SentenceId]] = {
    case class Loading(
      prevPrompts: List[QASRLEvaluationPrompt[SentenceId]],
      curLines: List[String])
    def processLinesIntoPrompt(lines: List[String]) = {
      val firstLine :: qaLines = lines
      val sentenceFields = firstLine.split("\t")
      val id = SentenceId.fromString(sentenceFields(0))
      val sentenceTokens = sentenceFields(1).split(" ").toVector
      val qaPairs = qaLines.map { qaLine =>
        val fields = qaLine.trim.split("\t")

        val verbIndex = fields(0).toInt

        val verbInflectedForms = inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase).get
        val slotsString = fields(2)
        val abstractedSlots = SlotBasedLabel.fromRenderedString(readVerbForm(_), ",")(slotsString).get
        val questionString = abstractedSlots.renderQuestionString(verbInflectedForms)

        val answerSpans = fields.drop(3).toList.map { s =>
          val Array(begin, end) = s.split("-").map(_.toInt)
          Span(begin, end)
        }

        VerbQA(verbIndex, questionString, answerSpans)
      }
      QASRLEvaluationPrompt(id, source, qaPairs)
    }
    val Loading(allPromptsButLast, lastLines) = loadInputFile(filename).get.foldLeft(Loading(Nil, Nil)) {
      case (Loading(prevPrompts, curLines), nextLine) =>
        if(nextLine.startsWith("\t") || curLines.isEmpty) {
          Loading(prevPrompts, curLines ++ List(nextLine))
        } else {
          val prompt = processLinesIntoPrompt(curLines)
          Loading(prompt :: prevPrompts, List(nextLine))
        }
    }
    (processLinesIntoPrompt(lastLines) :: allPromptsButLast).toVector.reverse
  }

  implicit object SentenceIdHasAlignedTokens extends HasTokens[SentenceId] {
    override def getTokens(id: SentenceId): Vector[String] = id match {
      case TQASentenceId(topicId, sentenceIndex) => tqaTexts(topicId).sentences(sentenceIndex).map(_.token)
      case WikiSentenceId(path) => Wiki1k.getSentence(path).get.tokens
    }
  }

  lazy val trainPrompts = readPromptsFromFile("test_expansion_validations.tsv", "heldout_test_0")
  lazy val trainPromptSet = trainPrompts.toSet
  lazy val devPrompts = readPromptsFromFile("test_dev_validations.tsv", "dev_test_0")
  lazy val devPromptSet = devPrompts.toSet
  lazy val allPrompts: Vector[QASRLEvaluationPrompt[SentenceId]] = weightedRoundRobinRandomized(
    List(trainPrompts, devPrompts),
    new Random(158631795L)
  )

  lazy val experiment = new QASRLEvaluationPipeline(
    allPrompts,
    frozenEvaluationHITTypeId = frozenEvaluationHITTypeId,
    validationAgreementDisqualTypeLabel = Some("eval-r1"))

  // import qasrl.labeling._

  // def saveAnnotationData[A](
  //   filename: String,
  //   ids: Vector[SentenceId],
  //   genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
  //   valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
  //   anonymizeWorker: String => String,
  //   labelMapper: QuestionLabelMapper[String, A],
  //   labelRenderer: A => String
  // ) = {
  //   saveOutputFile(
  //     s"$filename.tsv",
  //     DataIO.makeQAPairTSV(
  //       ids.toList,
  //       SentenceId.toString,
  //       genInfos,
  //       valInfos,
  //       // anonymizeWorker, // TODO not outputting worker ID in dataset right now
  //       labelMapper,
  //       labelRenderer)
  //   )
  // }

  // def saveAnnotationDataReadable(
  //   filename: String,
  //   ids: Vector[SentenceId],
  //   genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
  //   valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
  //   anonymizeWorker: String => String
  // ) = {
  //   saveOutputFile(
  //     s"$filename.tsv",
  //     DataIO.makeReadableQAPairTSV(
  //       ids.toList,
  //       SentenceId.toString,
  //       anonymizeWorker,
  //       genInfos,
  //       valInfos,
  //       (id: SentenceId, qa: VerbQA, responses: List[QASRLValidationAnswer]) => responses.forall(_.isAnswer))
  //   )
  // }

  // lazy val infos = experiment.allInfos
  // TODO reuse anonymizations from first round? maybe? eh
  // lazy val workerAnonymizationMap: Map[String, String] = {

  //   val allWorkerIds = valInfos.iterator.flatMap(_.assignments).map(_.workerId).toSet

  //   val rand = new scala.util.Random(1543754734L)
  //   val randomOrderedWorkerIds = rand.shuffle(allWorkerIds.toVector)
  //   randomOrderedWorkerIds.zipWithIndex.map {
  //     case (workerId, index) => workerId -> index.toString
  //   }.toMap
  // }

  // mainly just for reference, or if you need to re-produce all versions of the dataset
  // def writeAllTSVCombinations = {
  //   writeAllTSVs[LowerCaseString]("string", QuestionLabelMapper.mapToLowerCase, _.toString)
  //   writeAllTSVs[DiscreteLabel]("discrete", DiscreteLabel.getDiscreteLabels, _.render)
  //   writeAllTSVs[SlotBasedLabel[LowerCaseString]](
  //     "slots", SlotBasedLabel.getSlotsForQuestion, _.renderWithSeparator(identity, ","))
  //   writeAllTSVs[SlotBasedLabel[VerbForm]](
  //     "slots-tense", SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion, _.renderWithSeparator(_.toString.lowerCase, ","))
  //   writeReadableTSVs
  // }

  // def writeAllTSVs[Label](
  //   labelType: String,
  //   labelMapper: QuestionLabelMapper[String, Label],
  //   labelRenderer: Label => String
  // ) = {
  //   // train
  //   saveAnnotationData(s"$label/$labelType/train/tqa", tqaTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/train/wikipedia", wikipediaTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/train/wikinews", wikinewsTrainIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   // dev
  //   saveAnnotationData(s"$label/$labelType/dev/tqa", tqaDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/dev/wikipedia", wikipediaDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/dev/wikinews", wikinewsDevIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   // test
  //   saveAnnotationData(s"$label/$labelType/test/tqa", tqaTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/test/wikipedia", wikipediaTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/test/wikinews", wikinewsTestIds, genInfos, valInfos, workerAnonymizationMap, labelMapper, labelRenderer)
  // }

  // def writeReadableTSVs = {
  //   // train
  //   saveAnnotationDataReadable(s"$label/readable/train/tqa", tqaTrainIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/train/wikipedia", wikipediaTrainIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/train/wikinews", wikinewsTrainIds, genInfos, valInfos, workerAnonymizationMap)
  //   // dev
  //   saveAnnotationDataReadable(s"$label/readable/dev/tqa", tqaDevIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/dev/wikipedia", wikipediaDevIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/dev/wikinews", wikinewsDevIds, genInfos, valInfos, workerAnonymizationMap)
  //   // test
  //   saveAnnotationDataReadable(s"$label/readable/test/tqa", tqaTestIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/test/wikipedia", wikipediaTestIds, genInfos, valInfos, workerAnonymizationMap)
  //   saveAnnotationDataReadable(s"$label/readable/test/wikinews", wikinewsTestIds, genInfos, valInfos, workerAnonymizationMap)
  // }
}
