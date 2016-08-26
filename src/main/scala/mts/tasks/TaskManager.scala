package mts.tasks

import mts.core._
import mts.util._

import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.Cancellable

import upickle.default.Writer

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

  var schedule: Option[Cancellable] = None

  def start(): Unit = {
    if(schedule.isEmpty || schedule.get.isCancelled) {
      stop()
      schedule = Some(context.system.scheduler.schedule(
                        0 seconds,
                        interval,
                        self,
                        Update)(context.system.dispatcher, self))
    }
  }

  def stop(): Unit = {
    schedule.foreach(_.cancel())
  }


  // used to temporarily withdraw HITs from the system; an Update will re-extend them
  def expire(): Unit = {
    stop()
    service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(spec.hitType))
      .foreach(hit => {
                 service.forceExpireHIT(hit.getHITId())
                 println
                 println(s"Expired HIT: ${hit.getHITId()}")
                 println(s"HIT type for expired HIT: ${spec.hitType}")
               })
  }

  // delete all HITs from the system and forget about the results
  def disable(): Unit = {
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
    stop()
  }

  def update(): Unit = {
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

  def addPrompt(p: Prompt): Unit = {
    dataManager.addNewPrompt(p)
  }
}
