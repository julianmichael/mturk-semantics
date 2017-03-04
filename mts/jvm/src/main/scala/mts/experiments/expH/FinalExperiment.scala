package mts.experiments.expH

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.ptb._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.requester.Comparator

class FinalExperiment(implicit config: TaskConfig) {
  val experimentName = "h_final"

  val approvalRateRequirement = new QualificationRequirement(
    RequesterService.APPROVAL_RATE_QUALIFICATION_TYPE_ID,
    Comparator.GreaterThanOrEqualTo, 95,
    null, true)

  // saved these manually, see code in package.scala
  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](FileManager.loadDataFile(experimentName, "origQASRLPaths.txt").get)

  lazy val random250PTBSentencePathss = {
    val shuffleRand = new util.Random(987654321L)
    shuffleRand.shuffle(origQASRLPaths)
      .take(250)
  }

  val genHITType = HITType(
    title = s"Write questions and answers about a word in context",
    description = s"""
      Given a sentence and a succession of words from that sentence,
      write a questions and answers involving each word.
      Come up with more question-answer pairs for a bonus!
    """.trim,
    reward = 0.20,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](approvalRateRequirement))

  lazy val genApiFlow = Flow[GenerationApiRequest].map {
    case GenerationApiRequest(id) =>
      val sentence = getSentenceById(id)
      val wordStats: WordStats =
        sentence.indices.map(i => i -> WordStat.empty(i)).toMap // TODO get from the hit manager
      GenerationApiResponse(sentence, wordStats)
  }

  val sampleGenPrompt = GenerationPrompt(origQASRLPaths.head, List(0, 1, 2, 3))

  lazy val genTaskSpec = TaskSpecification[GenerationPrompt, List[WordedQAPair], GenerationApiRequest, GenerationApiResponse](
    TaskIndex.expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt)

  // validation task definition

  val valHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and several questions,
      highlight the part of the sentence that answers the question.
    """.trim,
    reward = 0.15,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](approvalRateRequirement))

  lazy val valApiFlow = Flow[ValidationApiRequest].map {
    case ValidationApiRequest(id) => ValidationApiResponse(getSentenceById(id))
  }

  val sampleValPrompt = ValidationPrompt(
    origQASRLPaths.head, List(
      SourcedQAPair("", "", 0, WordedQAPair(0, "Who is awesome?", Set(1, 2, 3, 4))),
      SourcedQAPair("", "", 1, WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9)))))

  lazy val valTaskSpec = TaskSpecification[ValidationPrompt, List[ValidationAnswer], ValidationApiRequest, ValidationApiResponse](
    TaskIndex.expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt)

  // TODO get the right prompts

  // hit management --- circularly defined so they can communicate

  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager: ActorRef = if(config.isProduction) {
    ??? // actorSystem.actorOf(Props(new QAGenHITManager(hitManagementHelper, 2, 60, sourcePrompts.iterator)))
  } else {
    ??? // actorSystem.actorOf(Props(new QAGenHITManager(hitManagementHelper, 1, 4, sourcePrompts.iterator)))
  }

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = if(config.isProduction) {
    ??? // actorSystem.actorOf(Props(new QAGenHITManager(hitManagementHelper, 2, 60, sourcePrompts.iterator)))
  } else {
    ??? // actorSystem.actorOf(Props(new QAGenHITManager(hitManagementHelper, 1, 4, sourcePrompts.iterator)))
  }

  import config.actorSystem

  lazy val server = new Server(List(genTaskSpec, valTaskSpec))
  lazy val genActor = actorSystem.actorOf(Props(new TaskManager(genHelper, genManager)))
  lazy val valActor = actorSystem.actorOf(Props(new TaskManager(valHelper, valManager)))

  import TaskManager._
  def start(interval: FiniteDuration = 1 minute) = {
    server
    genActor ! Start(interval)
    valActor ! Start(interval)
  }
  def stop() = {
    genActor ! Stop
    valActor ! Stop
  }
  def disable() = {
    genActor ! Disable
    valActor ! Disable
  }
  def expire() = {
    genActor ! Expire
    valActor ! Expire
  }
  def update() = {
    server
    genActor ! Update
    valActor ! Update
  }

  // convenience functions

  def allGenInfos = FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId)
  def allValInfos = FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId)

}
