package mts

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.util.ClientConfig

package object tasks {
  /**
    * TaskConfig contains the general configurable parameters of a Mechanical Turk project.
    * See descriptions of fields within.
    */
  sealed trait TaskConfig {
    /**
      * This RequesterService is the API hook with which we communicate with MTurk.
      * We need a different hook depending on whether we're in sandbox or production,
      * because it uses a different URL.
      */
    val service: RequesterService
    /**
      * externalSubmitURL is used by HTMLQuestion and ExternalQuestion tasks to submit assignments.
      * That is, if we want to provide our own HTML with which to render the task (as I do in all of my experiments),
      * instead of using the default "Submit HIT" button, we must make our own HTML form and embed it in the HIT.
      * That form then needs to submit to this URL.
      */
    val externalSubmitURL: String
    /**
      * The label will be either "sandbox" or "production" and it is used to segregate the saved data for each task
      * between sanbox and production runs.
      */
    val label: String
    /**
      * This boolean flag is just for convenience in cases where, for example, we don't want to upload 100 HITs to the sandbox
      * if we just want to try it out; or, we want only 1 assignment per HIT in sandbox mode so we can test assignment approval.
      */
    val isProduction: Boolean
  }

  /**
    * Convenience method to load configuration that is common to sandbox and production.
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

  private[this] object ProductionTaskConfig extends TaskConfig {
    private[this] val config = loadGlobalConfig()
    config.setServiceURL(ClientConfig.PRODUCTION_SERVICE_URL)
    override val service = new RequesterService(config)
    override val externalSubmitURL = "https://www.mturk.com/mturk/externalSubmit"
    override val label = "production"
    override val isProduction = true
  }

  private[this] object SandboxTaskConfig extends TaskConfig {
    private[this] val config = loadGlobalConfig()
    config.setServiceURL(ClientConfig.SANDBOX_SERVICE_URL)
    override val service = new RequesterService(config)
    override val externalSubmitURL = "https://workersandbox.mturk.com/mturk/externalSubmit"
    override val label = "sandbox"
    override val isProduction = false
  }

  // change this value and recompile to switch between sandbox and production
  val Config: TaskConfig = ProductionTaskConfig
}
