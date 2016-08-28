package mts.tasks

import mts.core.Assignment
import mts.core.HIT
import mts.util._

import scala.collection.mutable

import upickle.default.Reader

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
class PromptOnceDataManager[Prompt, Response](
  hitType: String,
  _promptSource: Iterator[Prompt],
  _finishedPrompts: Iterator[Prompt]
)(
  implicit pr: Reader[Prompt]
) extends DataManager[Prompt, Response] {

  private[this] val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  private[this] val finishedOrActivePrompts = {
    val set = mutable.Set.empty[Prompt]
    set ++= _finishedPrompts
    set ++= (for {
      mTurkHIT <- Config.service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitType)
      hit = FileManager.getHIT[Prompt](hitType, mTurkHIT.getHITId).toOptionPrinting.get
    } yield hit.prompt)
    set
  }

  final override def receiveAssignments(assignments: List[Assignment[Response]]): Unit =
    ()

  final override def nextPrompts(n: Int): List[Prompt] =
    queuedPrompts.filterPop(!finishedOrActivePrompts.contains(_), n)

  final override def promptSucceeded(hit: HIT[Prompt]): Unit =
    finishedOrActivePrompts += hit.prompt

  final override def promptFailed(prompt: Prompt): Unit =
    queuedPrompts.enqueue(prompt)

  final override def addNewPrompt(prompt: Prompt): Unit =
    queuedPrompts.enqueue(prompt)
}
