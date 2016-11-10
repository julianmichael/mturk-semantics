package mts.tasks

import mts.core._

import com.amazonaws.mturk.requester.AssignmentStatus

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef

abstract class HITManager[Prompt, Response](
  val taskSpec: TaskSpecification[Prompt, Response])(
  implicit config: TaskConfig
) {
  object Message {
    sealed trait Message
    case class Start(interval: FiniteDuration) extends Message
    case object Stop extends Message
    case object Update extends Message
    case object Expire extends Message
    case object Disable extends Message
    case class EvaluateAssignment(assignment: Assignment[Response], evaluation: AssignmentEvaluation) extends Message
    case class AddPrompt(prompt: Prompt) extends Message
  }

  /** Takes a reviewable HIT and does what must be done:
    * - approve/reject assignments as necessary. // TODO consider factoring this out into another method.
    * - extend the hit if necessary.
    * - dispose of the hit if necessary.
    * - notify other tasks of results if necessary.
    * - upload new HITs if necessary.
    */
  def reviewHIT(taskActor: ActorRef, hit: HIT[Prompt]): Unit

  def addPrompt(prompt: Prompt): Unit
}
