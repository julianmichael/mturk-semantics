package mts.tasks

import akka.actor.ActorSystem

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.util.ClientConfig
import com.amazonaws.mturk.dataschema.QuestionFormAnswersType

import upickle.default._

/** Contains the global configuration of our usage of the MTurk API,
  * including relevant values (URLs, API hooks) and whether we are running
  * on production or in the sandbox.
  */
sealed trait TaskConfig {
  /** The API hook with which we communicate with MTurk.
    * We need a different hook depending on whether we're in sandbox or production,
    * because it uses a different URL.
    */
  val service: RequesterService

  /** The URL used by HTMLQuestion and ExternalQuestion question types to submit assignments.
    * (See http://docs.aws.amazon.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_QuestionAnswerDataArticle.html
    * for documentation on these question types.)
    * In particular, if we want to provide our own HTML with which to render the task (which we usually do),
    * instead of using the default "Submit HIT" button, we must make our own HTML form and embed it in the HIT.
    * That form then needs to submit to this URL.
    */
  val externalSubmitURL: String

  /** Either "sandbox" or "production": used to segregate the saved data for each task
    * between sandbox and production runs.
    */
  val label: String

  /** Just for convenience in cases where, for example, we don't want to upload 100 HITs to the sandbox
    * just for trying out a task; or, we want only 1 assignment per HIT in sandbox mode so we can test assignment approval.
    */
  val isProduction: Boolean

  /** The ActorSystem we're using to manage tasks. */
  val actorSystem: ActorSystem

  /** The domain at which we're hosting our server. */
  val serverDomain: String

  /** The interface we're using to host the server. */
  val interface: String

  /** What port we're hosting HTTP at. */
  val httpPort: Int

  /** What port we're hosting HTTPS at. */
  val httpsPort: Int
}

object TaskConfig {
  /** Convenience method to load configuration that is common to sandbox and production.
    * In particular, this loads our API keys from the `mturk.properties` file.
    */
  protected[tasks] def loadGlobalConfig(): ClientConfig = {
    val config = new PropertiesClientConfig("mturk.properties")
    import scala.collection.JavaConverters._
    config.setRetriableErrors(Set("Server.ServiceUnavailable").asJava)
    config.setRetryAttempts(10)
    config.setRetryDelayMillis(1000L)
    config
  }
}

/** Complete configuration for running on production. */
case class ProductionTaskConfig(override val serverDomain: String) extends TaskConfig {
  private[this] val config = TaskConfig.loadGlobalConfig()
  config.setServiceURL(ClientConfig.PRODUCTION_SERVICE_URL)
  override val service = new RequesterService(config)
  override val externalSubmitURL = "https://www.mturk.com/mturk/externalSubmit"
  override val label = "production"
  override val isProduction = true

  override val actorSystem = ActorSystem(s"$serverDomain-$label")
  private[this] val akkaConfig = actorSystem.settings.config
  override val interface = akkaConfig.getString("app.interface")
  override val httpPort = akkaConfig.getInt("app.httpPort")
  override val httpsPort = akkaConfig.getInt("app.httpsPort")
}

/** Complete configuration for running on the sandbox. */
case class SandboxTaskConfig(override val serverDomain: String) extends TaskConfig {
  private[this] val config = TaskConfig.loadGlobalConfig()
  config.setServiceURL(ClientConfig.SANDBOX_SERVICE_URL)
  override val service = new RequesterService(config)
  override val externalSubmitURL = "https://workersandbox.mturk.com/mturk/externalSubmit"
  override val label = "sandbox"
  override val isProduction = false

  override val actorSystem = ActorSystem(s"$serverDomain-$label")
  private[this] val akkaConfig = actorSystem.settings.config
  override val interface = akkaConfig.getString("app.interface")
  override val httpPort = akkaConfig.getInt("app.httpPort")
  override val httpsPort = akkaConfig.getInt("app.httpsPort")
}
