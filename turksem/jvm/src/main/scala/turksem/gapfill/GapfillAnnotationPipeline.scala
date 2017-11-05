package turksem.gapfill

import cats.implicits._

import akka.actor.Props
import akka.stream.scaladsl.{Flow, Source}
import com.amazonaws.services.mturk.model.QualificationRequirement

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
import turkey.tasks.Server
import turkey.tasks.Service
import turkey.tasks.SetNumHITsActive

import turksem._
import turksem.util._

import upickle.default._

import scala.concurrent.duration._
import scala.language.postfixOps

import QuestionGuesserProducer.ops._

import LowerCaseStringSerialization._

/**
  * Annotation pipeline object: construct one of these to start running an annotation job.
  */
class GapfillAnnotationPipeline[
  SID : Reader : Writer : HasTokens,
  Guesser : Reader : Writer,
  GuesserProducer : Reader : Writer](
  val _prompts: Vector[GapfillPrompt[SID]],
  val initialGuesserProducer: GuesserProducer,
  frozenGenerationHITTypeID: Option[String] = None)(
  implicit config: TaskConfig, // determines production/sandbox, how to store HIT data, etc.
  annotationDataService: AnnotationDataService,
  inflections: Inflections,
  adaptiveGuesserInstance: QuestionGuesserProducer[GuesserProducer] { type ClientGuesser = Guesser; type SentenceId = SID }
) { // inflections object constructed by the caller for all tokens in the inputs

  def getInflectedTokens(sid: SID) = {
    val tokens = sid.tokens
    PosTagger.posTag(tokens).map(w =>
      InflectionalWord(
        token = w.token,
        pos = w.pos,
        index = w.index,
        inflectedFormsOpt = inflections.getInflectedForms(tokens(w.index).lowerCase)))
  }

  val allPrompts = _prompts.filter { p =>
    QuestioningState.initFromSentence(getInflectedTokens(p.id)).triggerGroups.nonEmpty
  }

  val genHITType = HITType(
    title = s"Help write and answer a series of questions about a sentence (2)",
    description = s"""
      You will be shown an English sentence and some simple questions about it.
      You will answer these questions, and aid an automated system in asking more questions
      and answering those as well, until you have covered as much of the sentence structure as possible.
    """.trim.replace("\\s+", " "),
    reward = 0.05,
    keywords = "language,english,question answering",
    qualRequirements = Array.empty[QualificationRequirement])

  lazy val genAjaxService = new Service[GapfillAjaxRequest[SID, Guesser]] {
    override def processRequest(request: GapfillAjaxRequest[SID, Guesser]) = request match {
      case GapfillAjaxRequest(id) =>
        val response = GapfillAjaxResponse(
          getInflectedTokens(id),
          genManagerPeek.guesserProducer.makeClientGuesser(id) // this is bad hehe oopsie
        )
        response
    }
  }

  lazy val sampleGenPrompt = allPrompts.head

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
  lazy val genTaskSpec = TaskSpecification.NoWebsockets[GapfillPrompt[SID], GapfillResponse, GapfillAjaxRequest[SID, Guesser]](
    gapfillTaskKey, genHITType, genAjaxService, sampleGenPrompt,
    frozenHITTypeId = frozenGenerationHITTypeID,
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks)

  import config.actorSystem

  // this is here just so you can peek from the console into what's going on in the HIT manager.
  // do NOT mutate fields of the HIT manager through this object---that is not thread-safe!
  // Instead, define some message types and send those messages to it.
  var genManagerPeek: GapfillHITManager[SID, GuesserProducer] = null

  // The HIT Manager keeps track of what's running on MTurk and reviews assignments.
  // You would implement any interesting quality control, coordination between tasks, etc., in a custom HITManager.
  // See the QAMR code for examples of more interesting HITManagers.
  // Here we are using a simple default implementation from the turkey library.
  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager = actorSystem.actorOf(
    Props{
      genManagerPeek = new GapfillHITManager(
            helper = genHelper,
            initGuesserProducer = initialGuesserProducer,
            numAssignmentsForPrompt = (p: GapfillPrompt[SID]) => 1,
            initNumHITsToKeepActive = 3,
            _promptSource = allPrompts.iterator)
      genManagerPeek
    })

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
    genActor ! Delete
  }
  def expire() = {
    genActor ! Expire
  }
  def update() = {
    server
    genActor ! Update
  }

  def allGenInfos = config.hitDataService.getAllHITInfo[GapfillPrompt[SID], GapfillResponse](genTaskSpec.hitTypeId).get

  def workerGenInfos(workerId: String) = for {
    hi <- allGenInfos
    assignment <- hi.assignments
    if assignment.workerId == workerId
  } yield HITInfo(hi.hit, List(assignment))

  def currentGenSentences: List[(SID, String)] = {
    genHelper.activeHITInfosByPromptIterator.map(_._1.id).map(id =>
      id -> Text.render(id.tokens)
    ).toList
  }
}
