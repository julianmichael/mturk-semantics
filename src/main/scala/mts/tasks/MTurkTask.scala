package mts.tasks

import mts.Annotation
import mts.Question
import mts.FileManager

import mts.qa.QASpec

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

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Cancellable
import scala.concurrent.duration._
import scala.language.postfixOps

trait MTurkTask {

  // final members and methods

  final val assignmentReviewPolicy = null // new ReviewPolicy("", Array.empty[PolicyParameter])
  final val hitReviewPolicy = null

  final lazy val hitType = service.registerHITType(
    autoApprovalDelay,
    assignmentDuration,
    reward,
    title,
    keywords,
    description,
    qualRequirements)

  final lazy val questionStore: mutable.Map[String, Question] = FileManager.loadQuestionStore(hitType) match {
    case None => mutable.Map.empty[String, Question]
    case Some(map) =>
      val result = mutable.Map.empty[String, Question]
      result ++= map.iterator
      result
  }


  final def hasEnoughFunds: Boolean = {
    val balance = service.getAccountBalance()
    // println("Got account balance: " + RequesterService.formatCurrency(balance))
    balance > reward
  }

  final def createHIT(question: Question): Try[HIT] = {
    Try {
      // just hash the time and main stuff of our request for the unique token.
      val uniqueRequestToken = (hitType, question.xml, System.nanoTime()).toString.hashCode.toString

      val hit: HIT = service.createHIT(
        hitType,
        title,
        description,
        keywords,
        question.xml,
        reward,
        assignmentDuration,
        autoApprovalDelay,
        lifetime,
        numAssignmentsPerHIT,
        question.annotation,
        qualRequirements,
        // Array.empty[String], //("Request.Minimal"), // response groups
        Array("Minimal", "HITQuestion", "HITDetail"), // response groups
        uniqueRequestToken,
        assignmentReviewPolicy,
        hitReviewPolicy
      )
      questionStore(hit.getHITId) = question // so we can retrieve the question later
      hit
    }
  }

  final def reviewHITs: List[Annotation] = for {
    // we could do this before the HITs are reviewable to make things slightly faster.
    // but it's ok.
    hit <- service.getAllReviewableHITs(hitType).toList
    annotation <- reviewHIT(hit)
  } yield annotation

  // for now, assumes that the HIT is reviewable.
  final def reviewHIT(hit: HIT): List[Annotation] = {
    if(hit.getHITStatus() == null || !hit.getHITStatus().equals("Reviewable")) {
      println
      System.err.println(s"reviewing a non-reviewable HIT. id: ${hit.getHITId()}; status: ${hit.getHITStatus()}")
    }

    val assignments = service.getAllAssignmentsForHIT(hit.getHITId())
    val submittedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
    val approvedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Approved))

    if(submittedAssignments.size + approvedAssignments.size < numAssignmentsPerHIT) {
      // If we don't have all of the assignments in, then extend the HIT.
      // The only reason it would be reviewable is if it had expired.
      service.extendHIT(hit.getHITId(), 1, lifetime)
      println
      println("Extended HIT: " + hit.getHITId());
      println("You may see your HIT with HITTypeId '" + hit.getHITTypeId() + "' here: ");
      println(service.getWebsiteURL() + "/mturk/preview?groupId=" + hit.getHITTypeId());

    } else if(approvedAssignments.size >= numAssignmentsPerHIT && submittedAssignments.size == 0) {
      // If we have approved enough assignments, and no more are in the queue, dispose of the HIT.
      service.disposeHIT(hit.getHITId())
      println
      println(s"Disposed HIT: ${hit.getHITId()}")
      println(s"HIT type of disposed: $hitType")
    }

    // Finally, review the submitted assignments and return the newly approved annotations in a list.
    for {
      assignment <- submittedAssignments.toList
      annotation <- reviewAssignment(hit, assignment)
    } yield annotation
  }

  /* Overridable or abstract fields */

  // Contract: returns Some(Annotation) if and only if we approve the assignment.
  // Otherwise returns None.
  // Override to do more interesting things if we don't want to auto-approve.
  def reviewAssignment(hit: HIT, assignment: Assignment): Option[Annotation] = {
    val question = questionStore.get(hit.getHITId()) match {
      case Some(q) => q
      case None => Question("Question lost.", "Question lost.")
    }
    val annotation = Annotation(hitType, hit.getHITId(), question,
                                assignment.getWorkerId(), assignment.getAnswer(),
                                assignment.getSubmitTime().getTime().getTime(),
                                assignment.getAssignmentId())
    FileManager.saveAnnotation(annotation) match {
      case Success(_) => // don't approve the assignment until we can successfully save the results
        service.approveAssignment(assignment.getAssignmentId(), "")
        questionStore.remove(hit.getHITId())
        println
        println(s"Approved assignment: ${assignment.getAssignmentId()}")
        println(s"HIT for approved assignment: ${hit.getHITId()}")
        println(s"HIT type for approved assignment: $hitType")
        Some(annotation)
      case Failure(e) =>
        println
        System.err.println(e.getLocalizedMessage())
        None
    }
  }

  // HIT type fields
  val title: String
  val description: String
  val keywords: String
  val reward: Double
  val autoApprovalDelay = 3600L // seconds (1 hour)
  val assignmentDuration = 120L // seconds (2 minutes)
  val qualRequirements = Array.empty[QualificationRequirement]

  // other fields
  val numAssignmentsPerHIT: Int
  val lifetime = 2592000L // seconds (30 days)

  val qaSpec: QASpec
  import qaSpec._

  def annotatedQAPairs = FileManager.loadAnnotationsForHITType(hitType)
    .groupBy(_.hitId)
    .map {
    case (hitId, annos) => (hitId -> annos.map(qaSpec.getQAPair))
  }

  def createMonitor(system: ActorSystem,
                    questions: Iterator[QuestionData],
                    numHITs: Int = 5,
                    interval: FiniteDuration = 1 minute
                    ): ActorRef = {
    system.actorOf(Props(Monitor(questions, numHITs, interval)))
  }

  object Messages {
    val Start = "start"
    val Stop = "stop"
    val Update = "update"
    val Expire = "expire"
  }

  // runs this MTurk task for the given data.
  // (the "given data" consists of the abstract fields)
  case class Monitor(
    val questions: Iterator[QuestionData],
    val numHITsToKeepActive: Int = 5,
    val interval: FiniteDuration = 1 minute
  ) extends Actor {

    import Messages._

    override def receive = {
      case Start => start()
      case Stop => stop()
      case Update => update()
      case Expire => expireAll()
      case _ => ()
    }

    override def postStop(): Unit = {
      FileManager.saveQuestionStore(hitType, questionStore.toMap)
    }

    val finishedQuestions = {
      val savedAnnotations = FileManager.loadAnnotationsForHITType(hitType)
      val set = mutable.HashSet[Question]()
      set ++= savedAnnotations.map(_.question)
      set
    }

    var remainingQuestions: Iterator[Question] = questions
      .map(createQuestion)
    // as more stuff gets added to finishedQuestionIds, this still ensures that
    // every time we pull something out of remainingQuestions,
    // it does not match a finishedQuestion.
      .filter(q => !finishedQuestions.contains(q))

    var schedule: Option[Cancellable] = None

    def expireAll(): Unit = {
      stop()
      service.searchAllHITs()
        .filter(hit => hit.getHITTypeId().equals(hitType))
        .foreach(hit => service.forceExpireHIT(hit.getHITId()))
    }

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

    def update(): Unit = {
      val newAnnotations = reviewHITs
      finishedQuestions ++= newAnnotations.map(_.question)

      val hitsOfThisType = service.searchAllHITs()
        .filter(hit => hit.getHITTypeId().equals(hitType))

      if(hitsOfThisType.size < numHITsToKeepActive) {
        if(remainingQuestions.hasNext) {
          val questionsToTry = remainingQuestions.take(numHITsToKeepActive - hitsOfThisType.size).toList
          val hitTries = questionsToTry.map(createHIT(_))
          hitTries.foreach (hitTry =>
            hitTry match {
              case Success(hit) =>
                println
                println("Created HIT: " + hit.getHITId());
                println("You may see your HIT with HITTypeId '" + hit.getHITTypeId() + "' here: ");
                println(service.getWebsiteURL() + "/mturk/preview?groupId=" + hit.getHITTypeId());
              case Failure(e) =>
                println
                System.err.println(e.getLocalizedMessage())
            })

          val newHITTries = questionsToTry.zip(hitTries)
          val failedQuestions = newHITTries.filter(_._2.isFailure).map(_._1)
          // put the failures back in the queue to try later
          remainingQuestions = remainingQuestions ++
            failedQuestions.iterator.filter(q => !finishedQuestions.contains(q))
        } else if(hitsOfThisType.size == 0) {
          stop()
        }
      }
    }
  }
}
