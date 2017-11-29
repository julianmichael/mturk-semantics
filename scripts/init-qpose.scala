import example.qpose._
import example.emnlp2017._
import turksem.gapfill._
import turksem.util._
import spacro._
import spacro.tasks._
import spacro.util._
import akka.pattern.ask
import scala.concurrent.duration._
import cats.implicits._

import com.amazonaws.services.mturk._
import com.amazonaws.services.mturk.model._

import nlpdata.util.Text
import nlpdata.util.HasTokens.ops._

val label = "trial"

val isProduction = false // sandbox. change to true for production
val domain = "localhost" // change to your domain, or keep localhost for testing
val projectName = "turksem-qpose" // make sure it matches the SBT project;
// this is how the .js file is found to send to the server

val annotationPath = java.nio.file.Paths.get(s"data/qpose/$label/annotations")
implicit val timeout = akka.util.Timeout(5.seconds)
val hitDataService = new FileSystemHITDataService(annotationPath)
implicit val config: TaskConfig = SandboxTaskConfig(projectName, domain, hitDataService)

def exit = {
  // actor system has to be terminated for JVM to be able to terminate properly upon :q
  config.actorSystem.terminate
  // flush & release logging resources
  import org.slf4j.LoggerFactory
  import ch.qos.logback.classic.LoggerContext
  LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext].stop
  System.out.println("Terminated actor system and logging. Type :q to end.")
}

val setup = new example.qpose.AnnotationSetup(label)

val exp = setup.experiment
exp.genManager
exp.server

def getId(i: Int) =
  setup.allPrompts(i).id

def getPromptInflectedTokens(i: Int) =
  exp.genAjaxService.processRequest(GapfillAjaxRequest(getId(i))).inflectedTokens

def getInitQState(i: Int) =
  QuestioningState(getPromptInflectedTokens(i), Vector())


// turk operations

// use with caution... intended mainly for sandbox
def deleteAll = {
  exp.setGenHITsActive(0)
  Thread.sleep(200)
  exp.expire
  // exp.delete
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
