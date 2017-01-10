package mts

/** Provides classes for managing tasks on Mechanical Turk.
  *
  * In particular, [[mts.tasks.TaskSpecification]] defines a HIT type,
  * how to create questions, and how to interpret answers.
  * [[mts.tasks.TaskManager]] coordinates API calls and gives an interface
  * for interacting with tasks on the console while running an experiment.
  * [[mts.tasks.DataManager]] coordinates which data is uploaded to MTurk as questions
  * and handles pre/post-processing of the data.
  */
package object tasks {
  val responseLabel = "response"
  val promptLabel = "prompt"
  val serverDomainLabel = "serverDomain"
  val httpsPortLabel = "httpsPort"
  val mturkFormLabel = "mturkForm"
  val rootClientDivLabel = "taskContent"
}
