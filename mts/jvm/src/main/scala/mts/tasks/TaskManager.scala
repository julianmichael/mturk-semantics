package mts.tasks

import mts.core._
import mts.util._

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.Cancellable

import upickle.default.Writer

/** Manages iteratively uploading, reviewing, disposing, and disabling HITs and assignments.
  *
  * A TaskManager, being an Actor, is designed to be interacted with asynchronously,
  * only by sending it messages (see the `receive` method).
  * Sending a TaskManager the Start method is the general way to begin an experiment,
  * or resume monitoring an experiment.
  * You generally will do this (or call a method which does this) on the SBT console.
  *
  * TODO: perhaps we should have TaskManager instantiate its own private DataManager,
  * just so it is more strictly enforced that a DataManager is owned by only one TaskManager.
  *
  * NOTE: there should only be one TaskManager for a TaskSpecification instance.
  * Enforcing this would be a bit annoying since a TaskManager must be created as an Actor
  * in an ActorSystem, but I don't wish for an ActorSystem to have exist for every TaskSpecification.
  *
  * @tparam Prompt data representation of an MTurk question
  * @tparam Response data representation of an annotator's response
  * @param spec the task specification for the task this is managing
  * @param dataManager the data manager for the task this is managing
  * @param numHITsToKeepActive the number of HITs that should be up on MTurk at one time
  * @param interval the time to wait between updates / API polls
  */
case class TaskManager[Prompt : Writer, Response : Writer](
  val spec: TaskSpecification[Prompt, Response],
  val dataManager: DataManager[Prompt, Response],
  val numHITsToKeepActive: Int = 100,
  val interval: FiniteDuration = 15 seconds) extends Actor {

  import Config._
  import spec.Message._

  override def receive = {
    case Start => start()
    case Stop => stop()
    case Expire => expire()
    case Disable => disable()
    case Update => update()
    case AddPrompt(p) => addPrompt(p)
  }

  // used to schedule updates once this has started
  private[this] var schedule: Option[Cancellable] = None

  // begin updating / polling the MTurk API
  private[this] def start(): Unit = {
    if(schedule.isEmpty || schedule.get.isCancelled) {
      schedule = Some(context.system.scheduler.schedule(
                        0 seconds,
                        interval,
                        self,
                        Update)(context.system.dispatcher, self))
    }
  }

  // stop regular polling
  private[this] def stop(): Unit = {
    schedule.foreach(_.cancel())
  }


  // temporarily withdraw HITs from the system; an update will re-extend them
  private[this] def expire(): Unit = {
    stop()
    service.searchAllHITs
      .filter(hit => hit.getHITTypeId().equals(spec.hitType))
      .foreach(hit => {
                 service.forceExpireHIT(hit.getHITId())
                 println
                 println(s"Expired HIT: ${hit.getHITId()}")
                 println(s"HIT type for expired HIT: ${spec.hitType}")
               })
  }

  // delete all HITs from the system (reviewing pending assignments) and forget about the results
  private[this] def disable(): Unit = {
    stop()
    // approve of finished tasks and collect the results first, just to prevent waste
    dataManager.receiveAssignments(spec.reviewHITs)
    service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(spec.hitType))
      .foreach(hit => {
                 service.disableHIT(hit.getHITId())
                 println
                 println(s"Disabled HIT: ${hit.getHITId()}")
                 println(s"HIT type for disabled HIT: ${spec.hitType}")
               })
  }

  // review assignments, dispose of completed HITs, and upload new HITs
  private[this] def update(): Unit = {
    println
    println(s"Updating (${spec.hitType})...")

    dataManager.receiveAssignments(spec.reviewHITs)

    val hitsOfThisType = service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(spec.hitType))

    if(hitsOfThisType.size < numHITsToKeepActive) {
      val promptsToTry = dataManager.nextPrompts(numHITsToKeepActive - hitsOfThisType.size)
      val hitTries = promptsToTry.zip(promptsToTry.map(prompt => spec.createHIT(prompt)))
      hitTries.foreach (hitTry =>
        hitTry match {
          case (prompt, Success(hit)) =>
            println
            println("Created HIT: " + hit.hitId);
            println("You may see your HIT with HITTypeId '" + hit.hitType + "' here: ");
            println(service.getWebsiteURL + "/mturk/preview?groupId=" + hit.hitType);
            dataManager.promptSucceeded(hit)
          case (prompt, Failure(e)) =>
            println
            System.err.println(e.getLocalizedMessage)
            e.printStackTrace
            dataManager.promptFailed(prompt)
        })
    }
  }

  // add a new prompt to the queue of questions to post to MTurk
  private[this] def addPrompt(p: Prompt): Unit = {
    dataManager.addNewPrompt(p)
  }
}
