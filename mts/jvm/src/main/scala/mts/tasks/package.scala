package mts

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.util.ClientConfig

/** Provides classes for managing tasks on Mechanical Turk.
  *
  * In particular, [[mts.tasks.TaskSpecification]] defines a HIT type,
  * how to create questions, and how to interpret answers.
  * [[mts.tasks.TaskManager]] coordinates API calls and gives an interface
  * for interacting with tasks on the console while running an experiment.
  * [[mts.tasks.DataManager]] coordinates which data is uploaded to MTurk as questions
  * and handles pre/post-processing of the data.
  *
  * This object contains the global configuration of our usage of the MTurk API,
  * including relevant values (URLs, API hooks) and deciding whether we run
  * on production or in the sandbox.
  */
package object tasks {

  /** Contains the configurable parameters of a Mechanical Turk project. */
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
      * between sanbox and production runs.
      */
    val label: String

    /** Just for convenience in cases where, for example, we don't want to upload 100 HITs to the sandbox
      * just for trying out a task; or, we want only 1 assignment per HIT in sandbox mode so we can test assignment approval.
      */
    val isProduction: Boolean
  }

  /** Convenience method to load configuration that is common to sandbox and production.
    * In particular, this loads our API keys from the `mturk.properties` file.
    */
  private[this] def loadGlobalConfig(): ClientConfig = {
    val config = new PropertiesClientConfig("mturk.properties")
    import scala.collection.JavaConverters._
    config.setRetriableErrors(Set("Server.ServiceUnavailable").asJava)
    config.setRetryAttempts(10)
    config.setRetryDelayMillis(1000L)
    config
  }

  /** Complete configuration for running on production. */
  private[this] object ProductionTaskConfig extends TaskConfig {
    private[this] val config = loadGlobalConfig()
    config.setServiceURL(ClientConfig.PRODUCTION_SERVICE_URL)
    override val service = new RequesterService(config)
    override val externalSubmitURL = "https://www.mturk.com/mturk/externalSubmit"
    override val label = "production"
    override val isProduction = true
  }

  /** Complete configuration for running on the sandbox. */
  private[this] object SandboxTaskConfig extends TaskConfig {
    private[this] val config = loadGlobalConfig()
    config.setServiceURL(ClientConfig.SANDBOX_SERVICE_URL)
    override val service = new RequesterService(config)
    override val externalSubmitURL = "https://workersandbox.mturk.com/mturk/externalSubmit"
    override val label = "sandbox"
    override val isProduction = false
  }

  /** The current configuration; decides whether we run on production or the sandbox. */
  val Config: TaskConfig = SandboxTaskConfig
}
