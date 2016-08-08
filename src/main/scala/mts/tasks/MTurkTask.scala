package mts.tasks

import mts.core._
import mts.util._
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

// TODO: make it so if we have SOME assignments saved for a HIT, but enough to meet our quota,
// we will send more HITs out to gather those annotations.

// TODO: add a facility for evaluating the quality of workers and banning them appropriately.

// TODO: add a facility for sending completed HITs to another actor for further processing (and perhaps turking)
trait MTurkTask {

  import Config._

  // final members and methods

  final val assignmentReviewPolicy = null
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
        Array("Minimal", "HITQuestion", "HITDetail"), // response groups --- these don't actually do anything
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

  // for now, we assume that the HIT is reviewable.
  // TODO: work even when it's not, just approving the submitted assignments.
  // This will be better for the case that we disable HITs that only have some of their submissions in.
  final def reviewHIT(hit: HIT): List[Annotation] = {
    // the HIT status seems to always be null, I suppose because of response groups not working.
    if(hit.getHITStatus() == null || !hit.getHITStatus().equals("Reviewable")) {
      // println
      // System.err.println(s"reviewing a non-reviewable HIT. id: ${hit.getHITId()}; status: ${hit.getHITStatus()}")
    }

    // first, get submissions and review them, retrieving the newly approved annotations and extending the HIT if necessary.
    val approvedAnnotations = {
      val assignments = service.getAllAssignmentsForHIT(hit.getHITId())
      val submittedAssignments = assignments.filter(a =>
        a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
      val approvedAssignments = assignments.filter(a =>
        a.getAssignmentStatus.equals(AssignmentStatus.Approved))

      if(submittedAssignments.size + approvedAssignments.size < numAssignmentsPerHIT) {
        // If we don't have all of the assignments in, then extend the HIT.
        // We do this because the only reason it would be reviewable is if it had expired.
        service.extendHIT(hit.getHITId(), 1, lifetime)
        println
        println("Extended HIT: " + hit.getHITId());
        println("You may see your HIT with HITTypeId '" + hit.getHITTypeId() + "' here: ");
        println(service.getWebsiteURL() + "/mturk/preview?groupId=" + hit.getHITTypeId());
      }

      // review the submitted assignments and return the newly approved annotations in a list.
      for {
        assignment <- submittedAssignments.toList
        annotation <- reviewAssignment(hit, assignment)
      } yield annotation
    }

    // now, check if the HIT is done and dispose of it if necessary.
    val assignments = service.getAllAssignmentsForHIT(hit.getHITId())
    val submittedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Submitted))
    val approvedAssignments = assignments.filter(a =>
      a.getAssignmentStatus.equals(AssignmentStatus.Approved))
    if(approvedAssignments.size >= numAssignmentsPerHIT && submittedAssignments.size == 0) {
      // If we have approved enough assignments, and no more are in the queue, dispose of the HIT.
      service.disposeHIT(hit.getHITId())
      questionStore.remove(hit.getHITId())
      println
      println(s"Disposed HIT: ${hit.getHITId()}")
      println(s"HIT type of disposed: $hitType")
    }

    approvedAnnotations
  }

  /* Overridable or abstract fields */

  // Contract: returns Some(Annotation) if and only if we approve the assignment.
  // Otherwise returns None.
  // Override to do more interesting things if we don't want to auto-approve.
  def reviewAssignment(hit: HIT, assignment: Assignment): Option[Annotation] = {
    // TODO: do something clever to save the approval time as well, maybe.
    val annotation = Annotation(assignmentId = assignment.getAssignmentId(),
                                hitType = hitType, hitId = hit.getHITId(),
                                question = questionStore.get(hit.getHITId()),
                                workerId = assignment.getWorkerId(),
                                answer = assignment.getAnswer(),
                                acceptTime = assignment.getAcceptTime().getTime().getTime(),
                                submitTime = assignment.getSubmitTime().getTime().getTime())
    FileManager.saveAnnotation(annotation) match {
      case Success(_) => // don't approve the assignment until we can successfully save the results
        service.approveAssignment(assignment.getAssignmentId(), "")
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
  val assignmentDuration = 600L // seconds (10 minutes)
  val qualRequirements = Array.empty[QualificationRequirement]

  // other fields
  val numAssignmentsPerHIT: Int
  val lifetime = 2592000L // seconds (30 days)

  val qaSpec: QASpec
  import qaSpec._

  def annotatedQAPairs() = FileManager.loadAnnotationsForHITType(hitType)
    .groupBy(_.hitId)
    .map {
    case (hitId, annos) => (hitId -> annos.flatMap(qaSpec.getQAPair))
  }

  def createMonitor(system: ActorSystem,
                    questions: Iterator[QuestionData],
                    numHITsToKeepActive: Int,
                    interval: FiniteDuration = 30 seconds
                    ): ActorRef = {
    system.actorOf(Props(Monitor(questions, numHITsToKeepActive, interval)))
  }

  sealed trait Message
  object Message {
    case object Start extends Message
    case object Stop extends Message
    case object Update extends Message
    case object Expire extends Message
    case object Disable extends Message
    case class AddQuestion(q: QuestionData) extends Message
  }

  // runs this MTurk task for the given data.
  // (the "given data" consists of the abstract fields)
  case class Monitor(
    val questionSource: Iterator[QuestionData],
    val numHITsToKeepActive: Int = 100,
    val interval: FiniteDuration = 10 seconds
  ) extends Actor {

    import Message._

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

    val finishedOrCurrentQuestions = {
      val savedAnnotations = FileManager.loadAnnotationsForHITType(hitType)
      val set = mutable.HashSet[QuestionData]()
      set ++= savedAnnotations.flatMap(_.question.map(qaSpec.extractQuestionData))
      set ++= questionStore.values.map(qaSpec.extractQuestionData)
      set
    }

    final val stackedQuestions: mutable.Stack[QuestionData] = mutable.Stack.empty[QuestionData]
    // questionSource (class param)
    final val failedQuestions: mutable.Queue[QuestionData] = mutable.Queue.empty[QuestionData]

    def getNextQuestion(): Option[QuestionData] = {
      val allQuestions = stackedQuestions.iterator ++ questionSource ++ failedQuestions.iterator
      val validQuestions = allQuestions.filter(q => !finishedOrCurrentQuestions.contains(q))
      validQuestions.nextOption
    }

    def getNextNQuestions(n: Int): Seq[QuestionData] = {
      val questions = mutable.Buffer.empty[QuestionData]
      var nRemaining = n
      while(nRemaining > 0) {
        getNextQuestion() match {
          case Some(q) =>
            questions += q
            nRemaining = nRemaining - 1
          case None =>
            nRemaining = 0
        }
      }
      questions.toVector
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
      finishedOrCurrentQuestions ++= newAnnotations.flatMap(_.question.map(qaSpec.extractQuestionData))

      val hitsOfThisType = service.searchAllHITs()
        .filter(hit => hit.getHITTypeId().equals(hitType))

      if(hitsOfThisType.size < numHITsToKeepActive) {
        val questionsToTry = getNextNQuestions(numHITsToKeepActive - hitsOfThisType.size)
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
              failedQuestions += question
          })
      }
    }

    def addQuestion(q: QuestionData): Unit = {
      stackedQuestions.push(q)
    }

  }
}
