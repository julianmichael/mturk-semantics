package mts.tasks

import mts.core.Assignment
import mts.core.HIT

trait DataManager[Prompt, Response] {
  def receiveAssignments(assignments: List[Assignment[Response]]): Unit
  def nextPrompts(n: Int): List[Prompt]
  def promptSucceeded(hit: HIT[Prompt]): Unit
  def promptFailed(prompt: Prompt): Unit
  def addNewPrompt(prompt: Prompt): Unit
}
