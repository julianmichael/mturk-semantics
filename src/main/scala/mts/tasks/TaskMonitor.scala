package mts.tasks

import mts.core._
import mts.util._

import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.service.exception.ServiceException
import com.amazonaws.mturk.util.PropertiesClientConfig
import com.amazonaws.mturk.requester.HIT
import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.requester.ReviewPolicy
import com.amazonaws.mturk.requester.PolicyParameter
import com.amazonaws.mturk.requester.HITLayoutParameter
import com.amazonaws.mturk.requester.Assignment
import com.amazonaws.mturk.requester.AssignmentStatus

import java.util.Calendar

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Cancellable

/**
  * XXX: this is an old class used just for experiments A and B. Don't use it anymore.
  * Instead use TaskManager with TaskSpecification.
  */

// runs this MTurk task for the given data.
// (the "given data" consists of the abstract fields)
case class TaskMonitor[Prompt, Response](
  val task: MTurkTask[Prompt, Response],
  val questionSource: Iterator[Prompt],
  val numHITsToKeepActive: Int = 100,
  val interval: FiniteDuration = 10 seconds
) extends Actor {

  import Config._
  import task._
  import task.Message._

  val finishedOrCurrentQuestions = {
    val savedAnnotations = FileManager.loadAnnotationsForHITType(hitType)
    val set = mutable.HashSet[Prompt]()
    set ++= savedAnnotations.flatMap(_.question.map(extractPrompt))
    set ++= questionStore.values.map(extractPrompt)
    set
  }

  val promptStackQueue: LazyStackQueue[Prompt] = new LazyStackQueue(questionSource)

  override def receive = {
    case Start => start()
    case Stop => stop()
    case Update => update()
    case Expire => expire()
    case Disable => disable()
    case AddQuestion(q) => addQuestion(q)
  }

  override def postStop(): Unit = {
    FileManager.saveQuestionStore(hitType, questionStore.toMap)
  }

  var schedule: Option[Cancellable] = None

  def start(): Unit = {
    if(schedule.isEmpty || schedule.get.isCancelled) {
      stop()
      schedule = Some(context.system.scheduler.schedule(
                        0 seconds,
                        interval,
                        self,
                        Update)(context.system.dispatcher, self))
    }
  }

  def stop(): Unit = {
    schedule.foreach(_.cancel())
  }


  // used to temporarily withdraw HITs from the system; an Update will re-extend them
  def expire(): Unit = {
    stop()
    service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(hitType))
      .foreach(hit => {
                 service.forceExpireHIT(hit.getHITId())
                 println
                 println(s"Expired HIT: ${hit.getHITId()}")
                 println(s"HIT type for expired HIT: $hitType")
               })
  }

  // delete all HITs from the system and forget about the results
  def disable(): Unit = {
    reviewHITs // approve of finished tasks and collect the results first, just to prevent waste
    service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(hitType))
      .foreach(hit => {
                 service.disableHIT(hit.getHITId())
                 println
                 println(s"Disabled HIT: ${hit.getHITId()}")
                 println(s"HIT type for disabled HIT: $hitType")
               })
    questionStore.clear()
    stop()
  }

  def update(): Unit = {
    println
    println(s"Updating ($hitType)...")

    val newAnnotations = reviewHITs
    finishedOrCurrentQuestions ++= newAnnotations.flatMap(_.question.map(extractPrompt))

    val hitsOfThisType = service.searchAllHITs()
      .filter(hit => hit.getHITTypeId().equals(hitType))

    if(hitsOfThisType.size < numHITsToKeepActive) {
      val questionsToTry = promptStackQueue.filterPop(x => !finishedOrCurrentQuestions.contains(x),
                                                      numHITsToKeepActive - hitsOfThisType.size)
      val hitTries = questionsToTry.zip(questionsToTry.map(qData => createHIT(createQuestion(qData))))
      hitTries.foreach (hitTry =>
        hitTry match {
          case (question, Success(hit)) =>
            println
            println("Created HIT: " + hit.getHITId());
            println("You may see your HIT with HITTypeId '" + hit.getHITTypeId() + "' here: ");
            println(service.getWebsiteURL() + "/mturk/preview?groupId=" + hit.getHITTypeId());
          case (question, Failure(e)) =>
            println
            System.err.println(e.getLocalizedMessage())
            promptStackQueue.enqueue(question)
        })
    }
  }

  def addQuestion(q: Prompt): Unit = {
    promptStackQueue.push(q)
  }

}
