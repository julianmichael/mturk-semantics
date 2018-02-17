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

val setup = new TQAEvaluationSetup(
  label,
  List(
    EvaluationInput("base/filtered/base_model_predictions.dev.filtered.tsv", "base")
  ),
  List(
    EvaluationInput("base/filtered/base_model_predictions.fold0.filtered.tsv", "base-fold0"),
    EvaluationInput("base/filtered/base_model_predictions.fold1.filtered.tsv", "base-fold1"),
    EvaluationInput("base/filtered/base_model_predictions.fold2.filtered.tsv", "base-fold2"),
    EvaluationInput("base/filtered/base_model_predictions.fold3.filtered.tsv", "base-fold3"),
    EvaluationInput("base/filtered/base_model_predictions.fold4.filtered.tsv", "base-fold4")
  ),
  validationAgreementDisqualTypeLabel = Some("eval")
)

import setup.SentenceIdHasAlignedTokens

val exp = setup.experiment
exp.server

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
