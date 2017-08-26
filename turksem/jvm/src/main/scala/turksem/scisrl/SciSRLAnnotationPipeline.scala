package turksem.scisrl

import cats.implicits._

import akka.actor.Props
import akka.stream.scaladsl.{Flow, Source}
import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.service.axis.RequesterService

import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.Inflections

import turkey.HITInfo
import turkey.HITType
import turkey.HIT
import turkey.Assignment
import turkey.tasks.TaskConfig
import turkey.tasks.TaskSpecification
import turkey.tasks.TaskManager
import turkey.tasks.HITManager
import turkey.tasks.NumAssignmentsHITManager
import turkey.tasks.Server
import turkey.tasks.SetNumHITsActive

import turksem._
import turksem.util._

import upickle.default._

import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Annotation pipeline object: construct one of these to start running an annotation job.
  */
class SciSRLAnnotationPipeline[SID : Reader : Writer : HasTokens](
  val allPrompts: Vector[SciSRLPrompt[SID]], // all inputs ("prompts") you wish to get outputs ("responses") for
  frozenGenerationHITTypeID: Option[String] = None)( // pass in Some("...") after completing the job for consistency in the future
  implicit config: TaskConfig, // determines production/sandbox, how to store HIT data, etc.
  inflections: Inflections) { // inflections object constructed by the caller for all tokens in the inputs

  // TODO: change the HIT type fields to what you desire. this is just copied from the QAMR stuff
  val genHITType = HITType(
    title = s"Write question-answer pairs about a sentence's meaning",
    description = s"""
      Given a sentence and some words from that sentence,
      write questions and answers involving each word.
      Write more question-answer pairs for increasing bonuses, and
      maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
    reward = SciSRLSettings.generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array.empty[QualificationRequirement])

  // this is the prompt shown when you preview the task at, e.g., localhost:8888?taskKey=generation
  lazy val sampleGenPrompt = allPrompts.head

  // add bootstrap to the page for some nicer styling
  lazy val (taskPageHeadLinks, taskPageBodyLinks) = {
    import scalatags.Text.all._
    val headLinks = List(
      link(
        rel := "stylesheet",
        href := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css",
        attr("integrity") := "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ",
        attr("crossorigin") := "anonymous"))
    val bodyLinks = List(
      script(
        src := "https://code.jquery.com/jquery-3.1.1.slim.min.js",
        attr("integrity") := "sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n",
        attr("crossorigin") := "anonymous"),
      script(
        src := "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js",
        attr("integrity") := "sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb",
        attr("crossorigin") := "anonymous"),
      script(
        src := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js",
        attr("integrity") := "sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn",
        attr("crossorigin") := "anonymous"))
    (headLinks, bodyLinks)
  }

  // this object holds the necessary information to start uploading tasks to Turk.
  // TaskSpecifications in an annotation run should be in 1-to-1 correspondence with HIT Type IDs.
  lazy val genTaskSpec = TaskSpecification[SciSRLPrompt[SID], SciSRLResponse, Unit, Unit](
    SciSRLSettings.generationTaskKey, genHITType, Flow[Unit], sampleGenPrompt,
    frozenHITTypeId = frozenGenerationHITTypeID,
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks)

  import config.actorSystem

  // this is here just so you can peek from the console into what's going on in the HIT manager.
  // do NOT mutate fields of the HIT manager through this object---that is not thread-safe!
  // Instead, define some message types and send those messages to it.
  var genManagerPeek: NumAssignmentsHITManager[SciSRLPrompt[SID], SciSRLResponse] = null

  // The HIT Manager keeps track of what's running on MTurk and reviews assignments.
  // You would implement any interesting quality control, coordination between tasks, etc., in a custom HITManager.
  // See the QAMR code for examples of more interesting HITManagers.
  // Here we are using a simple default implementation from the turkey library.
  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager = actorSystem.actorOf(
    Props(NumAssignmentsHITManager.constAssignments[SciSRLPrompt[SID], SciSRLResponse](
            helper = genHelper,
            numAssignmentsPerPrompt = 1,
            initNumHITsToKeepActive = 3,
            _promptSource = List(sampleGenPrompt).iterator)))

  // instantiating this object starts the webserver that hosts the task & previews.
  lazy val server = new Server(List(genTaskSpec))

  // this actor is the way we generally communicate directly with the HIT manager (telling it to start polling turk, stop, etc.)
  lazy val genActor = actorSystem.actorOf(Props(new TaskManager(genHelper, genManager)))

  // these functions are for you to run on the console to manage the task live.

  def setGenHITsActive(n: Int) =
    genManager ! SetNumHITsActive(n)

  import TaskManager.Message._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    genActor ! Start(interval, delay = 0 seconds)
  }
  def stop() = {
    genActor ! Stop
  }
  def disable() = {
    genActor ! Disable
  }
  def expire() = {
    genActor ! Expire
  }
  def update() = {
    server
    genActor ! Update
  }

  // these functions are for you to run on the console to check on the results that have been completed so far.
  // Ideally you could hook up the job to a web dashboard like I've done in the QAMR and QA-SRL projects.

  def allGenInfos = config.hitDataService.getAllHITInfo[SciSRLPrompt[SID], SciSRLResponse](genTaskSpec.hitTypeId).get

  def workerGenInfos(workerId: String) = for {
    hi <- allGenInfos
    assignment <- hi.assignments
    if assignment.workerId == workerId
  } yield HITInfo(hi.hit, List(assignment))

  def currentGenSentences: List[(SID, String)] = {
    genHelper.activeHITInfosByPromptIterator.map(_._1.sentenceId).map(id =>
      id -> Text.render(id.tokens)
    ).toList
  }
}
