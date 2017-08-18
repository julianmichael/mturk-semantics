import turksem.scisrl._
import turksem.util._
import turkey._
import turkey.tasks._
import akka.pattern.ask
import scala.concurrent.duration._

val isProduction = false // sandbox. change to true for production
val domain = "localhost" // change to your domain, or keep localhost for testing
val projectName = "turksem-scisrlexample" // make sure it matches the SBT project;
// this is how the .js file is found to send to the server

val annotationPath = java.nio.file.Paths.get("annotations")
implicit val timeout = akka.util.Timeout(5.seconds)
implicit val config: TaskConfig = {
  if(isProduction) {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("production"))
    ProductionTaskConfig(projectName, domain, hitDataService)
  } else {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("sandbox"))
    SandboxTaskConfig(projectName, domain, hitDataService)
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

val setup = new scisrlexample.SciSRLAnnotationSetup
val exp = setup.experiment
exp.server
