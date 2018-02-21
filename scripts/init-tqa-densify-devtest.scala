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

val label = "human-eval"

val isProduction = false // sandbox. change to true for production
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

def getHumanSentenceIds(filename: String) = {
  import scala.collection.JavaConverters._
  Files.lines(Paths.get("qasrl-data/human-sentences").resolve(filename)).iterator.asScala.map { line =>
    SentenceId.fromString(line.split("\t").head)
  }.toVector
}
val humanEvalFilenames = for {
  part <- List("dev", "test")
  domain <- List("tqa", "wikipedia", "wikinews")
} yield s"$part-$domain.tsv"

val idsForHumanEval = humanEvalFilenames.flatMap(getHumanSentenceIds)
val idStringsForHumanEval = idsForHumanEval.map(SentenceId.toString)
val idStringsForHumanEvalSet = idStringsForHumanEval.toSet

def keyIndicesMapFromFile(filename: String) = {
  import scala.collection.JavaConverters._
  Files.lines(Paths.get(filename)).iterator.asScala.map { line =>
    val fields = line.split("\t")
    SentenceId.fromString(fields(0)) -> fields.lift(2).fold(List.empty[Int])(_.split(" ").toList.map(_.toInt).sorted[Int])
  }.toMap
}

val allTokensFilenames = List("train", "dev", "test").map(name => s"qasrl-data/all-sentences/$name.tsv")

val keyIndicesMap = allTokensFilenames.foldMap(keyIndicesMapFromFile)

def allTokensIter = {
  def getSentenceTokensIter(filename: String) = {
    import scala.collection.JavaConverters._
    Files.lines(Paths.get(filename)).iterator.asScala.flatMap { line =>
      line.split("\t")(1).split(" ").iterator
    }
  }
  allTokensFilenames.iterator.flatMap(getSentenceTokensIter)
}

lazy val Wiktionary = new WiktionaryFileSystemService(
  Paths.get("resources").resolve("wiktionary")
)

lazy val inflections = Wiktionary.getInflectionsForTokens(allTokensIter)

def readVerbForm(lcs: LowerCaseString): VerbForm = lcs.toString match {
  case "stem" => Stem
  case "presentsingular3rd" => PresentSingular3rd
  case "presentparticiple" => PresentParticiple
  case "past" => Past
  case "pastparticiple" => PastParticiple
  case _ => ???
}

val threshold = 0.2

def readDatasetFromPromptFile(
  filename: String,
  source: String,
  inflections: Inflections
): QASRLDataset = {

  case class Loading(
    curEntries: Map[String, QASRLSentenceEntry],
    curSentenceEntryOpt: Option[QASRLSentenceEntry])

  def loadSentenceInfoFromLine(line: String) = {
    val Array(idString, tokensString) = line.split("\t")
    (idString, tokensString.split(" ").toVector)
  }

  val AnswerMatchToScoreString = "[0-9]+-[0-9]+\\[(.*)\\]".r

  def loadQuestionInfoFromLine(
    line: String,
    sentenceTokens: Vector[String]
  ) = {
    val fields = line.split("\t")

    val answerStrings = fields.toList.drop(4)
    val maxAnswerScore = answerStrings.map {
      case AnswerMatchToScoreString(scoreString) => scoreString.toDouble
      case x => println("uh oh!!!!!!! no match!!! " + x); 0.0
    }.max

    if(maxAnswerScore < threshold) None else Some {
      val verbIndex = fields(1).toInt
      val verbInflectedForms = inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase).get
      val modelString = fields(2).takeWhile(_ != ":")
      val slotsString = fields(3)
      val slots = SlotBasedLabel.fromRenderedString(readVerbForm(_), ",")(slotsString).get
      val questionString = slots.renderQuestionString(verbInflectedForms)
      QASRLLabel(
        QuestionLabel(
          Set(source),
          verbIndex,
          verbInflectedForms,
          questionString,
          slots),
        Set.empty
      )
    }
  }

  val filepath = Paths.get(filename)
  import scala.collection.JavaConverters._
  val lines = Files.lines(filepath).iterator.asScala
  val Loading(allEntriesButLast, lastEntryOpt) = lines.foldLeft(Loading(Map.empty, None)) {
    case (Loading(prevEntries, None), nextLine) => // assume line is sentence
      val (newSentenceIdString, newSentenceTokens) = loadSentenceInfoFromLine(nextLine)
      val newEntry = QASRLSentenceEntry(newSentenceIdString, newSentenceTokens, Nil)
      Loading(prevEntries, Some(newEntry))
    case (Loading(prevEntries, Some(curEntry)), nextLine) =>
      if(nextLine.startsWith("\t")) { // is QA pair
        loadQuestionInfoFromLine(nextLine, curEntry.sentenceTokens) match {
          case None =>
            Loading(prevEntries, Some(curEntry))
          case Some(newLabel) =>
            Loading(prevEntries, Some(QASRLSentenceEntry.labels.modify(newLabel :: _)(curEntry)))
        }
      } else { // is new sentence
        val newEntries = prevEntries + (
          curEntry.sentenceId -> QASRLSentenceEntry.labels.modify(_.reverse)(curEntry)
        )
        val (newSentenceIdString, newSentenceTokens) = loadSentenceInfoFromLine(nextLine)
        val newEntry = QASRLSentenceEntry(newSentenceIdString, newSentenceTokens, Nil)
        Loading(newEntries, Some(newEntry))
      }
  }
  lastEntryOpt.fold(QASRLDataset(allEntriesButLast))(lastEntry =>
    QASRLDataset(
      allEntriesButLast + (
        lastEntry.sentenceId -> QASRLSentenceEntry.labels.modify(_.reverse)(lastEntry)
      )
    )
  )
}

import QASRLDataset.JsonCodecs._

val devData = jawn.decodeFile[QASRLDataset](
  new File("qasrl-data/dev.json")
).right.get

val devDensifyData = devData.filterSentenceIds(idStringsForHumanEvalSet)

val devEvalData = readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/dev/base_local_output.dev.tsv", "base_local", inflections
) |+| readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/dev/base_output.dev.tsv", "base", inflections
) |+| readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/dev/expanded_output.dev.tsv", "extended", inflections
)

val devDataToAnnotate = devDensifyData |+| devEvalData
val allDevIdStrings = devDataToAnnotate.entries.keySet

val testData = jawn.decodeFile[QASRLDataset](
  new File("qasrl-data/test.json")
).right.get

val testDensifyData = testData.filterSentenceIds(idStringsForHumanEvalSet)

val testEvalData = readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/test/base_local_output.test.tsv", "base_local", inflections
) |+| readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/test/base_output.test.tsv", "base", inflections
) |+| readDatasetFromPromptFile(
  "qasrl-data/human-eval-input/test/expanded_output.test.tsv", "extended", inflections
)

val testDataToAnnotate = testDensifyData |+| testEvalData
val allTestIdStrings = testDataToAnnotate.entries.keySet

val allDataToAnnotate = devDataToAnnotate |+| testDataToAnnotate

def getPromptsWithNumNecessaryAnnotations(data: QASRLDataset, rand: Random) = {
  rand.shuffle(
    data.entries.iterator.flatMap { case (sidString, sentenceEntry) =>
      val id = SentenceId.fromString(sidString)
      sentenceEntry.labels.groupBy(_.answers.size).iterator
        .flatMap { case (numAnswers, labels) =>
          val numValidatorsNeeded = 6 - numAnswers
          rand.shuffle(
            labels.map(_.question).map {
              case QuestionLabel(sourceIds, verbIndex, _, question, _) =>
                SourcedQuestion(verbIndex, question, sourceIds)
            }
          ).grouped(8).map(qs =>
            QASRLEvaluationPrompt(id, qs.sortBy(_.verbIndex)) -> numValidatorsNeeded
          )
      }
    }.filter(_._2 > 0).toVector
  )
}

val rand = new Random(2819563L)

val promptsWithNumValidatorsNeeded = getPromptsWithNumNecessaryAnnotations(allDataToAnnotate, rand)
val allPrompts = promptsWithNumValidatorsNeeded.map(_._1)
val promptToNumValidatorsNeeded = promptsWithNumValidatorsNeeded.toMap

def numValidatorsForPrompt(prompt: QASRLEvaluationPrompt[SentenceId]): Int = {
  if(!isProduction) 1 else promptToNumValidatorsNeeded.get(prompt) match {
    case Some(n) => n
    case None =>
      System.err.println("mysterious prompt: " + prompt.toString)
      3
  }
}

val setup = new TQAEvaluationSetup(
  label,
  allPrompts,
  numValidatorsForPrompt(_),
  validationAgreementDisqualTypeLabel = Some("eval-final")
)

import setup.SentenceIdHasAlignedTokens

val exp = setup.experiment
exp.server

def datasets = {
  val dataExporter = new EvaluationDataExporter(exp)
  val fullDataset = dataExporter.dataset(SentenceId.toString, identity[String])
  val devDataset = fullDataset.filterSentenceIds(allDevIdStrings)
  val testDataset = fullDataset.filterSentenceIds(allTestIdStrings)
  (devDataset, testDataset)
}

def writeDataset(data: QASRLDataset, name: String) = {
  import io.circe.Printer
  setup.saveOutputFile(s"$name.json", Printer.noSpaces.pretty(data.asJson))
}

def writeAllDatasets = {
  val (dev, test) = datasets
  writeDataset(dev, "dev")
  writeDataset(test, "test")
}

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
