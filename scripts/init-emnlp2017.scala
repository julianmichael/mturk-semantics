import example.emnlp2017._
import example.emnlp2017.analysis._
import turksem.qamr._
import turksem.util._
import turkey._
import turkey.tasks._
import akka.pattern.ask
import scala.concurrent.duration._
import cats.implicits._
val annotationPath = java.nio.file.Paths.get("annotations")
implicit val timeout = akka.util.Timeout(5.seconds)
implicit val config: TaskConfig = {
  val isProduction = false // sandbox. change to true for production
  if(isProduction) {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("production"))
    ProductionTaskConfig("turksem-emnlp2017", "localhost", hitDataService)
  } else {
    val hitDataService = new FileSystemHITDataService(annotationPath.resolve("sandbox-emnlp2017"))
    SandboxTaskConfig("turksem-emnlp2017", "localhost", hitDataService)
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

val data = new DataExporter

// val setup = new emnlp2017.AnnotationSetup
// val exp = setup.experiment
// exp.server
