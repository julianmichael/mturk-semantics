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

case class EvaluationInput(
  filename: String,
  sourceId: String
)

class TQAEvaluationSetup(
  val label: String,
  allPrompts: Vector[QASRLEvaluationPrompt[SentenceId]],
  numValidatorsForPrompt: QASRLEvaluationPrompt[SentenceId] => Int,
  frozenEvaluationHITTypeId: Option[String] = None,
  validationAgreementDisqualTypeLabel: Option[String])(
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

  implicit object SentenceIdHasAlignedTokens extends HasTokens[SentenceId] {
    override def getTokens(id: SentenceId): Vector[String] = id match {
      case TQASentenceId(topicId, sentenceIndex) => tqaTexts(topicId).sentences(sentenceIndex).map(_.token)
      case WikiSentenceId(path) => Wiki1k.getSentence(path).get.tokens
    }
  }

  def allTokensIter = {
    allPrompts.iterator.map(_.id).toSet.iterator
      .flatMap((id: SentenceId) => id.tokens)
  }

  implicit lazy val inflections = Wiktionary.getInflectionsForTokens(allTokensIter)

  lazy val allIds = allPrompts.map(_.id).distinct

  lazy val experiment = new QASRLEvaluationPipeline(
    allPrompts,
    numValidatorsForPrompt,
    frozenEvaluationHITTypeId = frozenEvaluationHITTypeId,
    validationAgreementDisqualTypeLabel = validationAgreementDisqualTypeLabel)

  def data = new EvaluationDataExporter(experiment)

  def saveAnnotationDataTSV[A](
    filename: String,
    ids: Vector[SentenceId],
    infos: List[HITInfo[QASRLEvaluationPrompt[SentenceId], List[QASRLValidationAnswer]]],
    labelMapper: QuestionLabelMapper[String, A],
    labelRenderer: A => String
  ) = {
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeEvaluationQAPairTSV(
        ids.toList,
        SentenceId.toString,
        infos,
        labelMapper,
        labelRenderer)
    )
  }

  def writeSlotsTSV(
    filename: String,
    ids: Vector[SentenceId],
    infos: List[HITInfo[QASRLEvaluationPrompt[SentenceId], List[QASRLValidationAnswer]]]
  ) = saveAnnotationDataTSV(
    s"$label/slots-tense/$filename.tsv",
    ids,
    infos,
    SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion,
    (slots: SlotBasedLabel[VerbForm]) => slots.renderWithSeparator(_.toString.lowerCase, ",")
  )

  // def allInfos = dataExporter.infos
  // lazy val dataset = dataExporter.dataset(SentenceId.toString(_), identity[String])

  // lazy val trainIdStringsSet = trainIds.map(SentenceId.toString).toSet
  // lazy val devIdStringsSet = devIds.map(SentenceId.toString).toSet

  // lazy val trainDataset = dataset.filterSentenceIds(trainIdStringsSet)
  // lazy val devDataset = dataset.filterSentenceIds(devIdStringsSet)

  // def writeDatasets = {
  //   import io.circe.Printer
  //   import io.circe.syntax._
  //   import QASRLDataset.JsonCodecs._
  //   saveOutputFile(
  //     s"train/$label.json",
  //     Printer.noSpaces.pretty(trainDataset.asJson)
  //   )
  //   saveOutputFile(
  //     s"dev/$label.json",
  //     Printer.noSpaces.pretty(devDataset.asJson)
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
  //   // all
  //   saveAnnotationData(s"$label/$labelType/expand1-dev", devIds, allInfos, labelMapper, labelRenderer)
  //   saveAnnotationData(s"$label/$labelType/expand1-train", trainIds, allInfos, labelMapper, labelRenderer)
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
