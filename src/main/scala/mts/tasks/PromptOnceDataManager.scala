package mts.tasks

import mts.core.Assignment
import mts.core.HIT
import mts.util._

import scala.collection.mutable

import upickle.default.Reader

class PromptOnceDataManager[Prompt, Response](
  hitType: String,
  _promptSource: Iterator[Prompt],
  _finishedPrompts: Set[Prompt]
)(
  implicit pr: Reader[Prompt]
) extends DataManager[Prompt, Response] {

  // consumes promptSource---don't use the iterator directly elsewhere
  private[this] val queuedPrompts = new LazyStackQueue[Prompt](_promptSource)

  private[this] val finishedPrompts = {
    val set = mutable.Set.empty[Prompt]
    set ++= _finishedPrompts
    set
  }

  // assumes that we only receive assignments once the HIT is done
  final override def receiveAssignments(assignments: List[Assignment[Response]]): Unit = {
    val hitIDs = assignments.map(_.hitId).toSet
    for(hitId <- hitIDs) {
      val hit = FileManager.getHIT[Prompt](hitType, hitId).toOptionPrinting.get
      finishedPrompts += hit.prompt
    }
  }

  final override def nextPrompts(n: Int): List[Prompt] =
    queuedPrompts.filterPop(!finishedPrompts.contains(_), n)

  final override def promptSucceeded(hit: HIT[Prompt]): Unit =
    ()

  final override def promptFailed(prompt: Prompt): Unit =
    queuedPrompts.enqueue(prompt)

  final override def addNewPrompt(prompt: Prompt): Unit =
    queuedPrompts.enqueue(prompt)
}
