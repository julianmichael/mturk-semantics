package mts.experiments.expD

import mts.core.Assignment
import mts.core.HIT
import mts.util._
import mts.tasks._

import LowerCaseStrings._

import scala.collection.mutable

import upickle.default.Reader

class WordChoosingDataManager(
  hitType: String,
  numAssignmentsPerHIT: Int,
  bonus: Double,
  _promptSource: Iterator[WordChoosingPrompt],
  _initData: List[(HIT[WordChoosingPrompt], List[Assignment[WordChoosingResponse]])]
)(
  implicit pr: Reader[WordChoosingPrompt]
) extends DataManager[WordChoosingPrompt, WordChoosingResponse] {

  import Config._

  private[this] val hitCache: mutable.Map[String, HIT[WordChoosingPrompt]] = {
    val cache = mutable.Map.empty[String, HIT[WordChoosingPrompt]]
    for ((hit, _) <- _initData) cache.put(hit.hitId, hit)
    cache
  }

  // just to cache things and for convenience
  private[this] def getHIT(hitId: String): HIT[WordChoosingPrompt] = {
    if(hitCache.contains(hitId)) {
      hitCache(hitId)
    } else {
      val hit = FileManager.getHIT[WordChoosingPrompt](hitType, hitId).toOptionPrinting.get
      hitCache.put(hitId, hit)
      hit
    }
  }

  // consumes promptSource---don't use the iterator directly elsewhere
  private[this] val queuedPrompts = new LazyStackQueue[WordChoosingPrompt](_promptSource)

  val allPrompts: Counter[WordChoosingPrompt] = {
    Counter(_initData.flatMap {
              case (hit, assignments) => assignments.map(_ => hit.prompt)
            })
  }

  val finishedOrActivePrompts = {
    val set = mutable.Set.empty[WordChoosingPrompt]
    // finished ones
    set ++= (for {
      (prompt, count) <- allPrompts.iterator
      if count >= numAssignmentsPerHIT
    } yield prompt)
    // active ones
    set ++= (for {
      mTurkHIT <- service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitType)
      hit = getHIT(mTurkHIT.getHITId)
    } yield hit.prompt)
    set
  }

  // private[this] val activePrompts = {
  //   val set = mutable.Set.empty[Prompt]
  //   val activePaths = for {
  //     mTurkHIT <- service.searchAllHITs
  //     if mTurkHIT.getHITTypeId.equals(hitType)
  //     hit = getHIT(mTurkHIT.getHITId)
  //   } yield hit.prompt
  //   set ++= activePaths
  //   set
  // }

  final override def receiveAssignments(assignments: List[Assignment[WordChoosingResponse]]): Unit = {
    val hitIDs = assignments.map(_.hitId).toSet
    for(hitId <- hitIDs) {
      val hit = FileManager.getHIT[WordChoosingPrompt](hitType, hitId).toOptionPrinting.get
      allPrompts.add(hit.prompt)
      // has to be equals so we make sure to do it only once. Yes, funky stuff happens when we upload multiple HITs for same prompt
      if(allPrompts.get(hit.prompt) == numAssignmentsPerHIT) {
        // grant bonuses here. all assignments have been saved at this point, so...
        val assignments = FileManager.loadAssignmentsForHIT[WordChoosingResponse](hitType, hitId)
        for(assignment <- assignments) {
          val worker = assignment.workerId
          val otherQAWordPairs = for {
            a <- assignments
            if a.workerId != worker
            WordChoosingResponseItem(qWord, aWord, _, _) <- a.response.items
          } yield (qWord.lowerCase, aWord.lowerCase)
          val otherPairSet = otherQAWordPairs.toSet
          val workerPairs = assignment.response.items.map(item => (item.questionWord.lowerCase, item.answerWord.lowerCase))
          val rarePairOpt = workerPairs.find(!otherPairSet.contains(_))
          rarePairOpt match {
            case Some((qWord, aWord)) =>
              Config.service.grantBonus(
                worker,
                bonus,
                assignment.assignmentId,
                s"Unique question/answer word pair: $qWord / $aWord")
              println
              println(s"Paying bonus to worker $worker for assignment ${assignment.assignmentId}")
              println(s"HIT ID: ${assignment.hitId}; word pair: $qWord / $aWord")
            case None => ()
          }
        }
      }
    }
  }

  final override def nextPrompts(n: Int): List[WordChoosingPrompt] =
    queuedPrompts.filterPop(!finishedOrActivePrompts.contains(_), n)

  final override def promptSucceeded(hit: HIT[WordChoosingPrompt]): Unit =
    finishedOrActivePrompts += hit.prompt

  final override def promptFailed(prompt: WordChoosingPrompt): Unit =
    queuedPrompts.enqueue(prompt)

  final override def addNewPrompt(prompt: WordChoosingPrompt): Unit =
    queuedPrompts.enqueue(prompt)
}
