package mts.tasks

import mts.core._
import mts.util._

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable

import upickle.default.Writer
import upickle.default.Reader

/** Manages iteratively reviewing, disposing, and disabling HITs and assignments.
  * Essentially acts as the point of contact for a task,
  * if you want to give it new questions or reviewed assignments, etc.
  *
  * A TaskManager, being an Actor, is designed to be interacted with asynchronously,
  * only by sending it messages (see the `receive` method).
  * Sending a TaskManager the Start method is the general way to begin an experiment,
  * or resume monitoring an experiment.
  * You generally will do this (or call a method which does this) on the SBT console.
  *
  * @tparam Prompt data representation of an MTurk question
  * @tparam Response data representation of an annotator's response
  * @param taskSpec the specification for the task this is managing
  */
case class TaskManager[Prompt : Reader, Response : Writer](
  val hitManager: HITManager[Prompt, Response])(
  implicit config: TaskConfig
) extends Actor {

  val hitTypeId = hitManager.taskSpec.hitTypeId

  import config._
  import hitManager.Message._

  override def receive = {
    case Start(interval) => start(interval)
    case Stop => stop
    case Update => update
    case Expire => expire
    case Disable => disable
    case EvaluateAssignment(assignment, evaluation) => evaluateAssignment(assignment, evaluation)
    case AddPrompt(prompt) => addPrompt(prompt)
  }

  // used to schedule updates once this has started
  private[this] var schedule: Option[Cancellable] = None

  // begin updating / polling the MTurk API
  private[this] def start(interval: FiniteDuration): Unit = {
    if(schedule.isEmpty || schedule.get.isCancelled) {
      schedule = Some(context.system.scheduler.schedule(
                        0 seconds,
                        interval,
                        self,
                        Update)(context.system.dispatcher, self))
    }
  }

  // stop regular polling
  private[this] def stop: Unit = {
    schedule.foreach(_.cancel())
  }

  // temporarily withdraw HITs from the system; an update will re-extend them
  private[this] def expire: Unit = {
    stop
    service.searchAllHITs
      .filter(hit => hit.getHITTypeId().equals(hitTypeId))
      .foreach(hit => {
                 service.forceExpireHIT(hit.getHITId())
                 println
                 println(s"Expired HIT: ${hit.getHITId()}")
                 println(s"HIT type for expired HIT: ${hitTypeId}")
               })
  }

  // delete all HITs from the system (reviewing reviewable HITs and approving other pending assignments)
  private[this] def disable: Unit = {
    stop
    update
    expire
    // TODO get all currently pending assignments and manually approve them.
    // saving the results in another location. THEN disable all HITs
    service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(hitTypeId))
      .foreach(hit => {
                 service.disableHIT(hit.getHITId())
                 println
                 println(s"Disabled HIT: ${hit.getHITId()}")
                 println(s"HIT type for disabled HIT: ${hitTypeId}")
               })
  }

  // review assignments, dispose of completed HITs, and upload new HITs
  // the details are up to HITManager
  private[this] def update: Unit = {
    println
    println(s"Updating (${hitTypeId})...")
    hitManager.refreshHITs
    for(mTurkHIT <- service.getAllReviewableHITs(hitTypeId)) {
      FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting.map(reviewHIT _)
    }
  }

  // assumes HIT is reviewable
  private[this] def reviewHIT(hit: HIT[Prompt]): Unit = {
    hitManager.reviewHIT(self, hit)
  }

  private[this] def evaluateAssignment(assignment: Assignment[Response], evaluation: AssignmentEvaluation): Unit = evaluation match {
    case Approval(message) => FileManager.saveApprovedAssignment(assignment).toOptionPrinting match {
      case Some(_) =>
        service.approveAssignment(assignment.assignmentId, message)
        println
        println(s"Approved assignment: ${assignment.assignmentId}")
        println(s"HIT for approved assignment: ${assignment.hitId}; $hitTypeId")
      case None => // try again after 30 seconds
        context.system.scheduler.scheduleOnce(30 seconds, self, EvaluateAssignment(assignment, evaluation))(context.system.dispatcher, self)
    }
    case Rejection(message) => FileManager.saveRejectedAssignment(assignment).toOptionPrinting match {
      case Some(_) =>
        service.rejectAssignment(assignment.assignmentId, message)
        println
        println(s"Rejected assignment: ${assignment.assignmentId}")
        println(s"HIT for rejected assignment: ${assignment.hitId}; $hitTypeId")
        println(s"Reason: $message")
      case None => // try again after 30 seconds
        context.system.scheduler.scheduleOnce(30 seconds, self, EvaluateAssignment(assignment, evaluation))(context.system.dispatcher, self)
    }
  }

  private[this] def addPrompt(prompt: Prompt): Unit = hitManager.addPrompt(prompt)
}
