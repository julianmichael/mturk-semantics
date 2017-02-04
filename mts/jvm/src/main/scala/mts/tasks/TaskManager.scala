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

// back in time for old docs
// NOTE the hit manager needs to have the right type params ^
class TaskManager[Prompt, Response](
  hitManagementHelper: HITManager.Helper[Prompt, Response],
  hitManager: ActorRef
) extends Actor {

  import TaskManager._
  import hitManagementHelper.Message._
  import hitManagementHelper.taskSpec.hitTypeId
  import hitManagementHelper.config._

  override def receive = {
    case Start(interval) => start(interval)
    case Stop => stop
    case Update => update
    case Expire => expire
    case Disable => disable
  }

  // used to schedule updates once this has started
  private[this] var schedule: Option[Cancellable] = None

  // begin updating / polling the MTurk API
  private[this] def start(interval: FiniteDuration): Unit = {
    if(schedule.fold(true)(_.isCancelled)) {
      schedule = Some(context.system.scheduler.schedule(2 seconds, interval, self, Update)(context.system.dispatcher, self))
    }
  }

  // stop regular polling
  private[this] def stop: Unit = {
    schedule.foreach(_.cancel())
  }

  // temporarily withdraw HITs from the system; an may re-extend them or cause them to finish
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
    hitManager ! DisableAll
  }

  // review assignments, dispose of completed HITs, and upload new HITs
  private[this] def update: Unit = {
    println
    println(s"Updating (${hitTypeId})...")
    hitManager ! ReviewHITs
  }
}

object TaskManager {
  sealed trait Message
  case class Start(interval: FiniteDuration) extends Message
  case object Stop extends Message
  case object Update extends Message
  case object Expire extends Message
  case object Disable extends Message
}
