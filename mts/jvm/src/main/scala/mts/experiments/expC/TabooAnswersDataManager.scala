package mts.experiments.expC

import mts.core.Assignment
import mts.core.HIT
import mts.tasks._
import mts.conll._
import mts.util._

import scala.collection.mutable
import scala.collection.immutable.Vector
import scala.collection.immutable.Stream

import scala.util.Random

// not thread-safe. Should only belong to a TaskManager.
// TODO: refactor so that this is only instantiated within one.
class TabooAnswersDataManager(
  hitType: String,
  sentencePaths: Iterator[CoNLLSentencePath],
  initData: List[(HIT[TabooAnswersPrompt], List[Assignment[TabooAnswersResponse]])]
) extends DataManager[TabooAnswersPrompt, TabooAnswersResponse] {

  import Config._

  // how we know when a hit has "converged" based on the distribution of answers
  private[this] def isFinished(counter: Counter[String]) =
    (counter.sum > 8 && counter.median > 1.0) || counter.sum >= 30

  private[this] def pathIsFresh(path: CoNLLSentencePath): Boolean =
    !(finishedPaths.contains(path) ||
        inactiveSentenceStore.contains(path) ||
        activeSentenceStore.contains(path))

  val finishedPaths: mutable.Set[CoNLLSentencePath] = mutable.Set.empty[CoNLLSentencePath]

  // initialize with all saved annotations (initData) that aren't already finished collecting annotations
  val inactiveSentenceStore: QueueMap[CoNLLSentencePath, Counter[String]] = {
    val pairs = for {
      (hit, assignments) <- initData
      path = hit.prompt.path
      answers = for {
        assignment <- assignments
        (_, a) <- assignment.response.qaPairs
      } yield a.trim.toLowerCase.replaceAll("""[\p{Punct}]""", "")
    } yield (path, answers)
    val entries = pairs.groupBy(_._1).map {
      case (path, answerLists) => (path, Counter(answerLists.map(_._2).flatten.iterator))
    }
    val queueMap = QueueMap.empty[CoNLLSentencePath, Counter[String]]
    for((path, counter) <- entries) {
      if(isFinished(counter)) {
        finishedPaths.add(path)
      } else {
        queueMap.add(path, counter)
      }
    }
    queueMap
  }

  // initialize with everything uploaded to Turk, moving things over from the inactive counts map if necessary
  val activeSentenceStore: mutable.Map[CoNLLSentencePath, Counter[String]] = {
    val map = mutable.Map.empty[CoNLLSentencePath, Counter[String]]
    val activePaths = for {
      mTurkHIT <- service.searchAllHITs
      if mTurkHIT.getHITTypeId.equals(hitType)
      hit = FileManager.getHIT[TabooAnswersPrompt](hitType, mTurkHIT.getHITId).toOptionPrinting.get
    } yield hit.prompt.path
    for(path <- activePaths) {
      inactiveSentenceStore.remove(path) match {
        case Some(answerCounts) => map.put(path, answerCounts)
        case None => map.put(path, Counter[String]())
      }
    }
    map
  }

  // for now we're assuming we'll only have one assignment per HIT, so we can move the entry out of activeSentenceStore
  final override def receiveAssignments(assignments: List[Assignment[TabooAnswersResponse]]): Unit = {
    for(assignment <- assignments) {
      val hit = FileManager.getHIT[TabooAnswersPrompt](hitType, assignment.hitId).toOptionPrinting.get
      val path = hit.prompt.path
      val answerCounts = activeSentenceStore(path)
      activeSentenceStore.remove(path)
      for((_, a) <- assignment.response.qaPairs) {
        answerCounts.add(a.trim.toLowerCase.replaceAll("""[\p{Punct}]""", ""))
      }
      if(isFinished(answerCounts)) {
        finishedPaths.add(path)
      } else {
        inactiveSentenceStore.add(path, answerCounts)
      }
    }
  }

  // this is the difficult bit.. need to decide on the next sentence to upload, and with what taboo list.
  // really the hard part is prioritizing sentences and knowing when we're done
  final override def nextPrompts(n: Int): List[TabooAnswersPrompt] = {
    val maxTabooListSize = {
      val activeMax = if(activeSentenceStore.isEmpty) 0 else activeSentenceStore.values.map(_.size).max
      val inactiveMax = if(inactiveSentenceStore.isEmpty) 0 else inactiveSentenceStore.iterator.map(_._2.size).max
      math.max(activeMax, inactiveMax)
    }
    val activeTabooListSizeCounts = Counter(activeSentenceStore.values.map(_.size).iterator)
    val activeTabooListSizeDistribution = (0 to math.max(3, maxTabooListSize)).map(activeTabooListSizeCounts.get)
    val adjustedTabooListSizeDistribution = activeTabooListSizeDistribution
      .updated(0, activeTabooListSizeDistribution(0) - 1)
      .updated(1, activeTabooListSizeDistribution(1) - 1)
      .updated(2, activeTabooListSizeDistribution(2) - 1)
    def nextDesiredSizeStream(distr: IndexedSeq[Int]): Stream[Int] = {
      val min = distr.min
      val nextSize = distr.indexOf(min)
      nextSize #:: nextDesiredSizeStream(distr.updated(nextSize, min + 1))
    }
    val nextSizes = nextDesiredSizeStream(adjustedTabooListSizeDistribution)
    val sizeChoiceIter = nextSizes.iterator
    val prompts = mutable.Buffer.empty[TabooAnswersPrompt]
    while(prompts.size < n && (inactiveSentenceStore.size > 0 || sentencePaths.hasNext)) {
      val sizeChoice = sizeChoiceIter.next
      val potentialPrompt = inactiveSentenceStore.iterator.filter {
        case (path, counter) => counter.size == sizeChoice && !prompts.exists(_.path == path)
      }.nextOption
      potentialPrompt match {
        case None => if(sizeChoice == 0 && sentencePaths.hasNext) {
          var path = sentencePaths.next
          while(sentencePaths.hasNext && !pathIsFresh(path)) {
            path = sentencePaths.next
          }
          if(pathIsFresh(path)) {
            val prompt = TabooAnswersPrompt(path, List.empty[String])
            prompts += prompt
          }
        }
        case Some((path, counter)) =>
          val tabooList = counter.keyIterator.toList.sortBy(s => -counter.get(s))
            .take(counter.size / 2)
          // val tabooList = if(counter.size > 2) {
          //   val firstMin = counter.min
          //   val exclusionIter = Random.shuffle(
          //     counter.iterator.filter(_._2 == firstMin).toVector
          //   ).iterator.map(_._1)
          //   val firstExclude = exclusionIter.next
          //   val secondExclude = if(exclusionIter.hasNext) {
          //     exclusionIter.next
          //   } else {
          //     val secondMin = counter.iterator.collect {
          //       case (_, count) if count > firstMin => count
          //     }.min
          //     Random.shuffle(
          //       counter.iterator.filter(_._2 == secondMin).toVector
          //     ).head._1
          //   }
          //   counter.keyIterator.toList.filter(s =>
          //     !s.equals(firstExclude) && !s.equals(secondExclude))
          // } else {
          //   List.empty[String]
          // }
          val prompt = TabooAnswersPrompt(path, tabooList)
          prompts += prompt
      }
    }
    prompts.toList
  }

  final override def promptSucceeded(hit: HIT[TabooAnswersPrompt]): Unit = {
    val path = hit.prompt.path
    inactiveSentenceStore.remove(path) match {
      case None => activeSentenceStore.put(path, Counter[String]()) // it was a new sentence
      case Some(answerCounts) => activeSentenceStore.put(path, answerCounts)
    }
  }

  final override def promptFailed(prompt: TabooAnswersPrompt): Unit = {
    // to the back of the queue!
    val path = prompt.path
    inactiveSentenceStore.remove(path).foreach(inactiveSentenceStore.add(path, _))
  }

  final override def addNewPrompt(prompt: TabooAnswersPrompt): Unit = {
    val path = prompt.path
    // only add it if we don't already have it
    if(!inactiveSentenceStore.contains(path) && !activeSentenceStore.contains(path)) {
      inactiveSentenceStore.add(path, Counter[String]())
    }
  }
}
