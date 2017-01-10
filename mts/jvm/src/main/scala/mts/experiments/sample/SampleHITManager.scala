package mts.experiments.sample

import mts.core._
import mts.util._
import mts.tasks._

import com.amazonaws.mturk.requester.AssignmentStatus

import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef

class SampleHITManager[Prompt, Response](
  taskSpec: TaskSpecification[Prompt, Response])(
  implicit config: TaskConfig
) extends HITManager[Prompt, Response](taskSpec) {

  import Message._
  import config._
  import scala.collection.mutable

  // TODO use this. should it even be here?
  // final val (finishedPrompts, activePrompts): (mutable.Set[Prompt], mutable.Set[Prompt]) = {
  //   val allData = ???
  //   val activeHITs = service.searchAllHITs().filter(_.getHITTypeId == taskSpec.hitTypeId)
  //   // TODO
  //   val finished = mutable.Set.empty[Prompt]
  //   finished ++= allData -- activePrompts
  //   val active = mutable.Set.empty[Prompt]
  //   active ++= activePrompts
  //   (finished, active)
  // }

  /** Takes a reviewable HIT and does what must be done:
    * - approve/reject assignments as necessary. // TODO consider factoring this out into another method.
    * - extend the hit if necessary.
    * - dispose of the hit if necessary.
    * - notify other tasks of results if necessary.
    * - upload new HITs if necessary.
    */
  def reviewHIT(taskActor: ActorRef, hit: HIT[Prompt]): Unit = {
    val submittedAssignments = service.getAllAssignmentsForHIT(hit.hitId, Array(AssignmentStatus.Submitted))
    if(submittedAssignments.isEmpty) {
      // must either have all assignments done or have expired
      service.disposeHIT(hit.hitId)
    } else {
      // TODO should I do this?
      service.setHITAsReviewing(hit.hitId)

      for(mTurkAssignment <- submittedAssignments) {
        val assignment = Assignment(
          hitTypeId = taskSpec.hitTypeId,
          hitId = hit.hitId,
          assignmentId = mTurkAssignment.getAssignmentId,
          workerId = mTurkAssignment.getWorkerId,
          acceptTime = mTurkAssignment.getAcceptTime.getTime.getTime,
          submitTime = mTurkAssignment.getSubmitTime.getTime.getTime,
          response = taskSpec.extractResponse(mTurkAssignment.getAnswer),
          feedback = taskSpec.extractFeedback(mTurkAssignment.getAnswer))
        reviewAssignment(taskActor, assignment)
      }
    }
  }

  private[this] def reviewAssignment(taskActor: ActorRef, assignment: Assignment[Response]): Unit = {
    taskActor ! EvaluateAssignment(assignment, Approval(""))
  }

  def addPrompt(prompt: Prompt): Unit = {
    taskSpec.createHIT(prompt, 1).toOptionPrinting.foreach { hit =>
      println
      println(s"Created HIT: ${hit.hitId}")
      println(s"You can view it here: https://workersandbox.mturk.com/mturk/preview?groupId=${hit.hitTypeId}")
    }
  }
}
