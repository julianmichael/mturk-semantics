package mts.experiments.expC

import mts.core.Assignment
import mts.core.HIT
import mts.tasks._
import mts.conll._
import mts.util._

import scala.collection.mutable

// not thread-safe. Should only belong to a TaskManager.
// TODO: refactor so that this is only instantiated within one.
class TabooAnswersDataManager(
  hitType: String,
  sentencePaths: Iterator[CoNLLSentencePath]
) extends DataManager[TabooAnswersPrompt, TabooAnswersResponse] {

  import Config._

  private[this] val initData = FileManager.loadAllData[TabooAnswersPrompt, TabooAnswersResponse](hitType)

  private[this] val hitCache: mutable.Map[String, HIT[TabooAnswersPrompt]] = {
    val cache = mutable.Map.empty[String, HIT[TabooAnswersPrompt]]
    for ((hit, _) <- initData) cache.put(hit.hitId, hit)
    cache
  }

  // just to cache things and for convenience
  private[this] def getHIT(hitId: String): HIT[TabooAnswersPrompt] = {
    if(hitCache.contains(hitId)) {
      hitCache(hitId)
    } else {
      val hit = FileManager.getHIT[TabooAnswersPrompt](hitType, hitId).toOptionPrinting.get
      hitCache.put(hitId, hit)
      hit
    }
  }

  // initialize with all saved annotations (ie initData)
  private[this] val inactiveSentenceStore: mutable.Map[CoNLLSentencePath, Counter[String]] = {
    val pairs = for {
      (hit, assignments) <- initData
      path = hit.prompt.path
      answers = for {
        assignment <- assignments
        (_, a) <- assignment.response.qaPairs
      } yield a
    } yield (path, Counter(answers.iterator))
    val map = mutable.Map.empty[CoNLLSentencePath, Counter[String]]
    for((path, counter) <- pairs) {
      map.put(path, counter)
    }
    map
  }

  // initialize with everything uploaded to Turk, moving things over from the inactive counts map if necessary
  private[this] val activeSentenceStore: mutable.Map[CoNLLSentencePath, Counter[String]] = {
    val map = mutable.Map.empty[CoNLLSentencePath, Counter[String]]
    val activePaths = for {
      mTurkHIT <- service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitType)
      hit = getHIT(mTurkHIT.getHITId)
    } yield hit.prompt.path
    for(path <- activePaths) {
      if(inactiveSentenceStore.contains(path)) {
        val answerCounts = inactiveSentenceStore(path)
        inactiveSentenceStore.remove(path)
        map.put(path, answerCounts)
      } else {
        map.put(path, Counter[String]())
      }
    }
    map
  }

  // for now we're assuming we'll only have one assignment per HIT, so we can move the entry out of activeSentenceStore
  final override def receiveAssignments(assignments: List[Assignment[TabooAnswersResponse]]): Unit = {
    for(assignment <- assignments) {
      val hit = getHIT(assignment.hitId)
      val path = hit.prompt.path
      val answerCounts = activeSentenceStore(path)
      activeSentenceStore.remove(path)
      for((_, a) <- assignment.response.qaPairs) {
        answerCounts.add(a)
      }
      inactiveSentenceStore.put(path, answerCounts)
    }
  }

  // this is the difficult bit.. need to decide on the next sentence to upload, and with what taboo list.
  // really the hard part is prioritizing sentences and knowing when we're done
  final override def nextPrompts(n: Int): List[TabooAnswersPrompt] = {
    ???
  }

  final override def promptSucceeded(hit: HIT[TabooAnswersPrompt]): Unit = {
    val path = hit.prompt.path
    val answerCounts = inactiveSentenceStore(path)
    inactiveSentenceStore.remove(path)
    activeSentenceStore.put(path, answerCounts)
  }

  final override def promptFailed(prompt: TabooAnswersPrompt): Unit = {
    () // do nothing... just keep trying :/
  }

  final override def addNewPrompt(prompt: TabooAnswersPrompt): Unit = {
    val path = prompt.path
    // only add it if we don't already have it
    if(!inactiveSentenceStore.contains(path) && !activeSentenceStore.contains(path)) {
      inactiveSentenceStore.put(path, Counter[String]())
    }
  }
}
