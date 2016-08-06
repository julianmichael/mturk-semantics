package mts

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.util.ClientConfig

package object tasks {
  sealed trait TaskConfig {
    val service: RequesterService
    val externalSubmitURL: String
  }

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
  }

  private[this] object SandboxTaskConfig extends TaskConfig {
    private[this] val config = loadGlobalConfig()
    config.setServiceURL(ClientConfig.SANDBOX_SERVICE_URL)
    override val service = new RequesterService(config)
    override val externalSubmitURL = "https://workersandbox.mturk.com/mturk/externalSubmit"
  }

  // change this value to switch between sandbox and production
  val Config: TaskConfig = SandboxTaskConfig
}
