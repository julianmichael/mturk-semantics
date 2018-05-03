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
  validationAgreementDisqualTypeLabel: Option[String],
  frozenEvaluationHITTypeId: Option[String] = None,
  alternativePromptReaderOpt: Option[Reader[QASRLEvaluationPrompt[SentenceId]]] = None)(
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
    validationAgreementDisqualTypeLabel = validationAgreementDisqualTypeLabel,
    alternativePromptReaderOpt = alternativePromptReaderOpt)

  lazy val data = new EvaluationDataExporter(experiment)
  // lazy val dataset = data.dataset(SentenceId.toString, identity)

  def writeDataset(name: String, dataset: QASRLDatasetNew.QASRLDataset) = {
    import io.circe.Printer
    import io.circe.syntax._
    import QASRLDatasetNew.QASRLDataset.JsonCodecs._
    saveOutputFile(
      s"$name/$label.jsonl",
      dataset.sentences.toVector.sortBy(_._1)
        .map(_._2.asJson)
        .map(Printer.noSpaces.pretty)
        .mkString("\n")
    )
  }

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

// import qasrl.Frame
// import qasrl.QuestionProcessor
// import qasrl.TemplateStateMachine
// import qasrl.labeling.QuestionLabelMapper

// import qasrl.crowd.util.CategoricalDistribution
// import qasrl.crowd.util.implicits._

// import cats.Foldable
// import cats.data.NonEmptyList
// import cats.implicits._

// import spacro.HITInfo
// import spacro.util.Span

// import nlpdata.datasets.wiktionary.Inflections
// import nlpdata.datasets.wiktionary.InflectedForms
// import nlpdata.util.HasTokens.ops._
// import nlpdata.util.LowerCaseStrings._
// import nlpdata.util.HasTokens
// import nlpdata.util.Text

// def makeFinalEvaluationQAPairTSV[QuestionLabel](
//   ids: List[SentenceId],
//   writeId: SentenceId => String, // serialize sentence ID for distribution in data file
//   infos: List[HITInfo[QASRLEvaluationPrompt[SentenceId], List[QASRLValidationAnswer]]],
//   existingData: QASRLDataset,
//   mapLabels: QuestionLabelMapper[String, QuestionLabel],
//   renderLabel: QuestionLabel => String)(
//   implicit inflections: Inflections
// ): String = {
//   val infosBySentenceId = infos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
//   // val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
//   // val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
//   val sb = new StringBuilder
//   for(id <- ids) {
//     val idString = writeId(id)
//     val sentenceTokens = id.tokens
//     val sentenceSB = new StringBuilder
//     var shouldIncludeSentence = false // for now, print everything
//     sentenceSB.append(s"${idString}\t${sentenceTokens.mkString(" ")}\n")
//     val turkQATuplesByVerbIndex = {
//       val qas = for {
//         HITInfo(hit, assignments) <- infosBySentenceId(id)
//         (sourcedQuestion, answers) <- hit.prompt.sourcedQuestions.zip(assignments.map(_.response).transpose)
//       } yield (sourcedQuestion, answers)
//       qas.groupBy(_._1.verbIndex)
//     }
//     val oldQALabelsByVerbIndex = existingData.entries(SentenceId.toString(id)).labels.groupBy(_.question.verbIndex)
//     for {
//       verbIndex <- (turkQATuplesByVerbIndex.keySet union oldQALabelsByVerbIndex.keySet).toList.sorted
//       inflForms <- inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase).toList
//       qasrlLabels = oldQALabelsByVerbIndex(verbIndex)
//       labels = mapLabels(sentenceTokens, inflForms, qasrlLabels.map(_.question.questionString))
//       (qasrlLabel, Some(qLabel)) <- qasrlLabels.zip(labels)
//     } yield {
//       val newLabelOpt = turkQATuplesByVerbIndex(verbIndex)
//         .find(t => t._1.question == qasrlLabel.question.questionString && t._1.sources == qasrlLabel.question.sourceIds)
//       val newAnswers = newLabelOpt.fold(List.empty[QASRLValidationAnswer])(_._2)
//       val oldAnswers = qasrlLabel.answers.toList.map(_.judgment)
//       val question = qasrlLabel.question.questionString
//       val sources = qasrlLabel.question.sourceIds
//       // SourcedQuestion(verbIndex, question, sources)
//       val answers = newAnswers ++ oldAnswers
//       // val valResp= answers.flatMap(_.getAnswer).map(_.spans)
//       if(answers.size == 6) {
//         shouldIncludeSentence = true
//         sentenceSB.append("\t")
//         sentenceSB.append(verbIndex.toString + "\t")
//         sentenceSB.append(sources.mkString(";") + "\t")
//         sentenceSB.append(renderLabel(qLabel) + "\t")
//         sentenceSB.append(
//           answers.map {
//             case Answer(spans) => spans
//                 .map(span => s"${span.begin}-${span.end}")
//                 .mkString(";")
//             case InvalidQuestion => "<Invalid>"
//           }.mkString("\t")
//         )
//         sentenceSB.append("\n")
//       } else {
//         import QASRLDataset.JsonCodecs._
//         import io.circe.syntax._
//         System.err.println(s"Missing some responses for: ${SentenceId.toString(id)}; num: ${answers.size}" )
//         System.err.println(Text.render(id))
//         System.err.println(question)
//         System.err.println(newAnswers + " ### " + oldAnswers)
//         System.err.println(io.circe.Printer.spaces2.pretty(existingData.filterQASRLLabels(_ == qasrlLabel).asJson))
//       }
//     }
//     if(shouldIncludeSentence) {
//       sb.append(sentenceSB.toString)
//     }
//   }
//   sb.toString
// }

// def saveFinalAnnotationDataTSV[A](
//   filename: String,
//   ids: Vector[SentenceId],
//   infos: List[HITInfo[QASRLEvaluationPrompt[SentenceId], List[QASRLValidationAnswer]]],
//   existingData: QASRLDataset,
//   labelMapper: QuestionLabelMapper[String, A],
//   labelRenderer: A => String
// ) = {
//   setup.saveOutputFile(
//     s"$filename.tsv",
//     makeFinalEvaluationQAPairTSV(
//       ids.toList,
//       SentenceId.toString,
//       infos,
//       existingData,
//       labelMapper,
//       labelRenderer)
//   )
// }

// def writeFinalSlotsTSV(
//   filename: String,
//   ids: Vector[SentenceId],
//   infos: List[HITInfo[QASRLEvaluationPrompt[SentenceId], List[QASRLValidationAnswer]]],
//   existingData: QASRLDataset
// ) = saveFinalAnnotationDataTSV(
//   s"$label/slots-tense/$filename",
//   ids,
//   infos,
//   existingData,
//   SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion,
//   (slots: SlotBasedLabel[VerbForm]) => slots.renderWithSeparator(_.toString.lowerCase, ",")
// )

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
