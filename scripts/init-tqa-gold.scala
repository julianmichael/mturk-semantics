import example.tqa._
import turksem.qamr._
import turksem.qasrl._
import turksem.util._
import turkey._
import turkey.tasks._
import akka.pattern.ask
import scala.concurrent.duration._

import com.amazonaws.services.mturk._
import com.amazonaws.services.mturk.model._

import nlpdata.util.Text
import nlpdata.util.HasAlignedTokens.ops._

val label = "gold"

val isProduction = false // sandbox. change to true for production
val domain = "nlp.cs.washington.edu" // change to your domain, or keep localhost for testing
val projectName = "turksem-tqa" // make sure it matches the SBT project;
// this is how the .js file is found to send to the server

val annotationPath = java.nio.file.Paths.get(s"data/tqa/$label/annotations")
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

val setup = new TQAAnnotationSetup(label)
import setup.TQASentenceIdHasAlignedTokens

val exp = setup.experiment
exp.server

// use with caution... intended mainly for sandbox
def deleteAll = {
  exp.setGenHITsActiveEach(0)
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

def get100ActiveHITIds = {
  config.service.listHITs(new ListHITsRequest().withMaxResults(100)).getHITs.asScala.toList.map(_.getHITId)
}
