package mts.tasks

import mts.core.Assignment
import mts.core.HIT

/** Handles pre/post-processing of assignments and decides which HITs to upload to MTurk.
  *
  * See [[mts.tasks.PromptOnceDataManager]] for a simple example implementation.
  *
  * Generally will maintain a (mutable) store of what prompts have been uploaded or completed,
  * in order to make sure redundant ones won't be uploaded to MTurk as new HITs.
  * This is necessary in order to maintain idempotence when restarting the program
  * while an experiment is running.
  * TODO: evaluate building some of this common functionality into DataManager itself.
  *
  * Inside a DataManager is where to, for example,
  * track when all assignments for a HIT have been received, do post-processing on the responses,
  * and dole out bonuses (as in [[mts.experiments.expD.WordChoosingDataManager]])
  * or pass off the results to be prompts for another task.
  *
  * An instance of a DataManager is held by a [[mts.tasks.TaskManager]],
  * who calls DataManager methods at the appropriate times
  * after getting new data from MTurk or receiving messages from other actors.
  *
  * @tparam Prompt data representation of an MTurk question
  * @tparam Response data representation of an annotator's response
  */
trait DataManager[Prompt, Response] {

  /** Executes post-processing of assignments that have been approved.
    *
    * The given assignments are assumed to have been saved to disk.
    * This method may, for example, determine when a HIT is completed,
    * process the results and dole out bonus payments,
    * or process the responses into new prompts and send them to another TaskManager
    * to run in another experiment in parallel.
    *
    * @param assignments already-approved assignments
    */
  def receiveAssignments(assignments: List[Assignment[Response]]): Unit

  /** Gets the next n prompts that should be uploaded to MTurk as new HITs.
    *
    * May return a list of < n prompts, if < n remain before we're done.
    * It is the responsibility of this method to determine that the prompts
    * returned are not redundant.
    *
    * @param n the max number of prompts to return
    * @return at most n prompts
    */
  def nextPrompts(n: Int): List[Prompt]

  /** Notification that a prompt was successfully uploaded as a HIT.
    *
    * This is a way to keep track of what HITs in particular are uploaded.
    * The given HIT is assumed to have been saved to disk.
    *
    * @param hit a just-uploaded HIT
    */
  def promptSucceeded(hit: HIT[Prompt]): Unit

  /** Notification that a prompt failed to upload as a HIT.
    *
    * This may be because of network issues or because the data was formatted incorrectly.
    * For the latter reason it may be ideal to replace the prompt at the back of the queue,
    * or choose not to reattempt uploading it as a HIT.
    *
    * @param prompt a prompt that failed to upload
    */
  def promptFailed(prompt: Prompt): Unit

  /** Request to add a new prompt to the experiment.
    *
    * This is called directly as a result of sending an AddPrompt message to a TaskManager.
    * Its use case is if you have a multi-stage experiment, where the prompts of one are
    * derived from the responses of the other (like a pair of QA generation and validation).
    *
    * @param prompt a new prompt to add to the experiment
    */
  def addNewPrompt(prompt: Prompt): Unit
}
