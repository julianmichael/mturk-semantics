import example.tqa._
import qasrl.crowd._
import qasrl.labeling._
import turksem.util._
import spacro._
import spacro.tasks._
import spacro.util._
import akka.pattern.ask
import scala.concurrent.duration._

import com.amazonaws.services.mturk._
import com.amazonaws.services.mturk.model._

import nlpdata.datasets.wiktionary._
import nlpdata.util.Text
import nlpdata.util.HasAlignedTokens.ops._
import nlpdata.util.LowerCaseStrings._

import java.io.File
import java.nio.file.{Paths, Files}

import io.circe.jawn
import io.circe.syntax._

import scala.util.Random

import cats.implicits._

val label = "expand1"

val isProduction = true // sandbox. change to true for production
val domain = "localhost" // change to your domain, or keep localhost for testing
val projectName = "turksem-tqaeval" // make sure it matches the SBT project;
// this is how the .js file is found to send to the server

val interface = "0.0.0.0"
val httpPort = 8888
val httpsPort = 8080

val annotationPath = java.nio.file.Paths.get(s"data/tqaeval/$label/annotations")
implicit val timeout = akka.util.Timeout(5.seconds)
implicit val config: TaskConfig = {
  if(isProduction) {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("production"))
    ProductionTaskConfig(projectName, domain, interface, httpPort, httpsPort, hitDataService)
  } else {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("sandbox"))
    SandboxTaskConfig(projectName, domain, interface, httpPort, httpsPort, hitDataService)
  }
}

def exit = {
  // actor system has to be terminated for JVM to be able to terminate properly upon :q
  config.actorSystem.terminate
  // flush & release logging resources
  import org.slf4j.LoggerFactory
  import ch.qos.logback.classic.LoggerContext
  LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop
  System.out.println("Terminated actor system and logging. Type :q to end.")
}

def allTokensForExpansionIter = {
  val filenames = List(
    "qasrl-data/base_filtered/base_model_predictions.dev.filtered.tsv",
    "qasrl-data/base_filtered/base_model_predictions.fold0.filtered.tsv",
    "qasrl-data/base_filtered/base_model_predictions.fold1.filtered.tsv",
    "qasrl-data/base_filtered/base_model_predictions.fold2.filtered.tsv",
    "qasrl-data/base_filtered/base_model_predictions.fold3.filtered.tsv",
    "qasrl-data/base_filtered/base_model_predictions.fold4.filtered.tsv"
  )
  filenames.iterator.flatMap { filename =>
    import scala.collection.JavaConverters._
    Files.lines(Paths.get(filename)).iterator.asScala
      .filter(l => !l.startsWith("\t"))
      .flatMap(_.split("\t").last.split(" ").iterator)
  }
}

lazy val Wiktionary = new WiktionaryFileSystemService(
  Paths.get("resources").resolve("wiktionary")
)

lazy val inflectionsForPromptReading = Wiktionary.getInflectionsForTokens(allTokensForExpansionIter)

def readVerbForm(lcs: LowerCaseString): VerbForm = lcs.toString match {
  case "stem" => Stem
  case "presentsingular3rd" => PresentSingular3rd
  case "presentparticiple" => PresentParticiple
  case "past" => Past
  case "pastparticiple" => PastParticiple
  case _ => ???
}

def readPromptsFromFile(filename: String, source: String): Vector[QASRLEvaluationPrompt[SentenceId]] = {
  case class Loading(
    prevPrompts: List[QASRLEvaluationPrompt[SentenceId]],
    curLines: List[String])
  def processLinesIntoPrompt(lines: List[String]) = {
    val firstLine :: qaLines = lines
    val sentenceFields = firstLine.split("\t")
    val id = SentenceId.fromString(sentenceFields(0))
    val sentenceTokens = sentenceFields(1).split(" ").toVector
    val sourcedQuestions = qaLines.map { qaLine =>
      val fields = qaLine.trim.split("\t")

      val verbIndex = fields(0).toInt

      val verbInflectedForms = inflectionsForPromptReading.getInflectedForms(sentenceTokens(verbIndex).lowerCase).get
      val slotsString = fields(2)
      val abstractedSlots = SlotBasedLabel.fromRenderedString(readVerbForm(_), ",")(slotsString).get
      val questionString = abstractedSlots.renderQuestionString(verbInflectedForms)

      SourcedQuestion(verbIndex, questionString, Set(source))
    }
    QASRLEvaluationPrompt(id, sourcedQuestions)
  }
  val filepath = Paths.get(filename)
  import scala.collection.JavaConverters._
  val lines = Files.lines(filepath).iterator.asScala
  val Loading(allPromptsButLast, lastLines) = lines.foldLeft(Loading(Nil, Nil)) {
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

val rand = new Random(13856932L)
val trainExpandPrompts = rand.shuffle(
    readPromptsFromFile("qasrl-data/base_filtered/base_model_predictions.fold0.filtered.tsv", "base-fold0") ++
    readPromptsFromFile("qasrl-data/base_filtered/base_model_predictions.fold1.filtered.tsv", "base-fold1") ++
    readPromptsFromFile("qasrl-data/base_filtered/base_model_predictions.fold2.filtered.tsv", "base-fold2") ++
    readPromptsFromFile("qasrl-data/base_filtered/base_model_predictions.fold3.filtered.tsv", "base-fold3") ++
    readPromptsFromFile("qasrl-data/base_filtered/base_model_predictions.fold4.filtered.tsv", "base-fold4")
)
val trainExpandPromptSet = trainExpandPrompts.toSet
val trainExpandIds = trainExpandPromptSet.map(_.id)
val trainExpandIdStrings = trainExpandIds.map(SentenceId.toString)

val devExpandPrompts = rand.shuffle(
  readPromptsFromFile(
    "qasrl-data/base_filtered/base_model_predictions.dev.filtered.tsv",
    "base")
)
val devExpandPromptSet = devExpandPrompts.toSet
val devExpandIds = devExpandPromptSet.map(_.id)
val devExpandIdStrings = devExpandIds.map(SentenceId.toString)

val allPrompts = weightedRoundRobinRandomized(
  List(trainExpandPrompts, devExpandPrompts),
  rand)

def numValidatorsForPrompt(prompt: QASRLEvaluationPrompt[SentenceId]): Int = {
  if(isProduction) 3 else 1
}

case class EvaluationPromptProxy(
  id: SentenceId,
  sourceId: String,
  qaPairs: List[VerbQA])

val altReader: upickle.default.Reader[QASRLEvaluationPrompt[SentenceId]] = upickle.default.Reader[QASRLEvaluationPrompt[SentenceId]] {
  case json: upickle.Js.Value =>
    val proxy = implicitly[upickle.default.Reader[EvaluationPromptProxy]].read(json)
    val sourcedQuestions = proxy.qaPairs.map(qa => SourcedQuestion(qa.verbIndex, qa.question, Set(proxy.sourceId)))
    QASRLEvaluationPrompt(proxy.id, sourcedQuestions)
}

val setup = new TQAEvaluationSetup(
  label,
  allPrompts,
  numValidatorsForPrompt(_),
  validationAgreementDisqualTypeLabel = Some("eval"),
  frozenEvaluationHITTypeId = Some("3KAKS2ULSEKYGWG5TZ89Y40G8QVTMM"),
  alternativePromptReaderOpt = Some(altReader)
)

import setup.SentenceIdHasAlignedTokens
// import QASRLDatasetNew.QASRLDataset.JsonCodecs._

val exp = setup.experiment
exp.server

def datasets = {
  val dataExporter = new EvaluationDataExporter(exp)
  val fullDataset = dataExporter.dataset(SentenceId.toString, identity[String])
  val trainDataset = fullDataset.filterSentenceIds(trainExpandIdStrings)
  val devDataset = fullDataset.filterSentenceIds(devExpandIdStrings)
  (trainDataset, devDataset)
}

// def writeDataset(data: QASRLDataset, name: String) = {
//   import io.circe.Printer
//   setup.saveOutputFile(s"$name.json", Printer.noSpaces.pretty(data.asJson))
// }

// def writeAllDatasets = {
//   val (train, dev) = datasets
//   writeDataset(train, "train")
//   writeDataset(dev, "dev")
// }

// use with caution... intended mainly for sandbox
def deleteAll = {
  exp.setValHITsActive(0)
  Thread.sleep(200)
  exp.expire
  exp.delete
}

def yesterday = {
  val cal = java.util.Calendar.getInstance
  cal.add(java.util.Calendar.DATE, -1)
  cal.getTime
}

import scala.collection.JavaConverters._

def expireHITById(hitId: String) = {
  config.service.updateExpirationForHIT(
    (new UpdateExpirationForHITRequest)
      .withHITId(hitId)
      .withExpireAt(yesterday))
}

def approveAllAssignmentsByHITId(hitId: String) = for {
  mTurkAssignment <- config.service.listAssignmentsForHIT(
    new ListAssignmentsForHITRequest()
      .withHITId(hitId)
      .withAssignmentStatuses(AssignmentStatus.Submitted)
    ).getAssignments.asScala.toList
} yield config.service.approveAssignment(
  new ApproveAssignmentRequest()
    .withAssignmentId(mTurkAssignment.getAssignmentId)
    .withRequesterFeedback(""))

def deleteHITById(hitId: String) =
  config.service.deleteHIT((new DeleteHITRequest).withHITId(hitId))

def disableHITById(hitId: String) = {
  expireHITById(hitId)
  deleteHITById(hitId)
}

def getActiveHITIds = {
  config.service.listAllHITs.map(_.getHITId)
}
