package mts.tasks

import mts.core._
import mts.util._

import scala.collection.mutable

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus

/** Simple example DataManager.
  *
  * Runs through all of the prompts in the source, skipping ones that have already been finished
  * or are still active.
  * When a prompt fails, puts it at the back of the queue to try again at the end.
  *
  * @tparam Prompt the data representation of an MTurk question
  * @tparam Response the data representation of an annotator's response
  * @param hitType the HIT Type ID of the task this is managing data for
  * @param _promptSource iterator over the desired prompts to turn into questions
  * @param _finishedPrompts iterator over prompts that have already been completed and/or should be skipped
  */
class PromptOnceHITManager[Prompt, Response](
  taskSpec: TaskSpecification[Prompt, Response],
  numAssignmentsPerHIT: Int,
  numHITsActive: Int,
  _promptSource: Iterator[Prompt],
  _finishedPrompts: Iterator[Prompt]
)(
  implicit pr: Reader[Prompt],
  config: TaskConfig
) extends HITManager[Prompt, Response](taskSpec) {

  import config._
  import Message._

  private[this] val hitTypeId = taskSpec.hitTypeId

  private[this] val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  private[this] val finishedOrActivePrompts = {
    val set = mutable.Set.empty[Prompt]
    set ++= _finishedPrompts
    set ++= (for {
      mTurkHIT <- config.service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitTypeId)
      hit = FileManager.getHIT[Prompt](hitTypeId, mTurkHIT.getHITId).toOptionPrinting.get
    } yield hit.prompt)
    set
  }

  // final override def nextPrompts(n: Int): List[Prompt] =
  //   queuedPrompts.filterPop(!finishedOrActivePrompts.contains(_), n)

  // final override def promptSucceeded(hit: HIT[Prompt]): Unit =
  //   finishedOrActivePrompts += hit.prompt

  // final override def promptFailed(prompt: Prompt): Unit =
  //   queuedPrompts.enqueue(prompt)

  /** Takes a reviewable HIT and does what must be done:
    * - approve/reject assignments as necessary. // TODO consider factoring this out into another method.
    * - extend the hit if necessary.
    * - dispose of the hit if necessary.
    * - notify other tasks of results if necessary.
    * - upload new HITs if necessary.
    */
  // TODO actually implement this fully...
  final override def reviewHIT(taskActor: ActorRef, hit: HIT[Prompt]): Unit = {
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

  final override def addPrompt(prompt: Prompt): Unit =
    queuedPrompts.enqueue(prompt)
}
