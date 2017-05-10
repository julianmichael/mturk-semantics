package turkey

/** Provides classes for managing tasks on Mechanical Turk.
  *
  * In particular, [[mts.tasks.TaskSpecification]] defines a HIT type,
  * how to create questions, and how to interpret answers.
  * [[mts.tasks.TaskManager]] coordinates API calls and gives an interface
  * for interacting with tasks on the console while running an experiment.
  * [[mts.tasks.DataManager]] coordinates which data is uploaded to MTurk as questions
  * and handles pre/post-processing of the data.
  */
package object tasks extends PackagePlatformExtensions {
  sealed trait HeartbeatingWebSocketMessage[+A]
  case object Heartbeat extends HeartbeatingWebSocketMessage[Nothing]
  case class WebSocketMessage[A](content: A) extends HeartbeatingWebSocketMessage[A]

  val responseLabel = "response"
  val promptLabel = "prompt"
  val serverDomainLabel = "serverDomain"
  val httpPortLabel = "httpPort"
  val httpsPortLabel = "httpsPort"
  val mturkFormLabel = "mturkForm"
  val rootClientDivLabel = "taskContent"
  val taskKeyLabel = "taskKey"
  val feedbackLabel = "feedback"
  val submitButtonLabel = "submitButton"
  val bottomDivLabel = "bottomDiv"
}
