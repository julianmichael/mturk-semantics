package turksem.qamr.annotation

import akka.actor._
import akka.stream.scaladsl.{Flow, Source}
import com.amazonaws.mturk.requester._
import com.amazonaws.mturk.service.axis.RequesterService

import nlpdata.util.Text

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr._

import upickle.default._

import scala.concurrent.duration._
import scala.language.postfixOps

class AnnotationPipeline[SID : Reader : Writer : HasTokens](
  val allIds: Vector[SID], // IDs of sentences to annotate
  numGenerationAssignmentsForPrompt: GenerationPrompt[SID] => Int,
  annotationDataService: AnnotationDataService,
  isStopword: IsStopword,
  qualTest: QualTest,
  frozenGenerationHITTypeID: Option[String] = None,
  frozenValidationHITTypeID: Option[String] = None,
  generationAccuracyQualTypeLabel: Option[String] = None,
  validationAgreementQualTypeLabel: Option[String] = None,
  validationTestQualTypeLabel: Option[String] = None)(
  implicit config: TaskConfig) {

  implicit val ads = annotationDataService
  implicit val is = isStopword
  implicit val settings = QAMRSettings
  import settings._

  import config.hitDataService

  val approvalRateRequirement = new QualificationRequirement(
    RequesterService.APPROVAL_RATE_QUALIFICATION_TYPE_ID,
    Comparator.GreaterThanOrEqualTo, 95,
    null, true)

  val locationRequirement = new QualificationRequirement(
    RequesterService.LOCALE_QUALIFICATION_TYPE_ID,
    Comparator.EqualTo, null,
    new Locale("US"), true)

  val genAccQualTypeLabelString = generationAccuracyQualTypeLabel.fold("")(x => s"[$x] ")
  val genAccQualTypeName = s"${genAccQualTypeLabelString}Question-answer writing accuracy % (auto-granted)"
  val genAccQualType = config.service.searchQualificationTypes(
    genAccQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 10
  ).getQualificationType.wrapNullable.flatMap(_.find(_.getName == genAccQualTypeName)).getOrElse {
    System.out.println("Generating generation qualification type...")
    config.service.createQualificationType(
      genAccQualTypeName,
      "language,english,question answering",
      """The rate at which questions provided for our
       question-answer generation task were judged
       valid and non-redundant, using the input of validators.""".replaceAll("\\s+", " "),
      QualificationTypeStatus.Active,
      null, // retry delay (seconds)
      null, null, null, // these 3 are for a test/answer key
      true, // auto granted
      101 // auto granted value
    )
  }
  val genAccQualTypeId = genAccQualType.getQualificationTypeId
  val genAccuracyRequirement = new QualificationRequirement(
    genAccQualTypeId,
    Comparator.GreaterThanOrEqualTo, (math.round(generationAccuracyBlockingThreshold * 100.0).toInt),
    null, false)

  val valAgrQualTypeLabelString = validationAgreementQualTypeLabel.fold("")(x => s"[$x] ")
  val valAgrQualTypeName = s"${valAgrQualTypeLabelString}Question answering agreement % (auto-granted)"
  val valAgrQualType = config.service.searchQualificationTypes(
    valAgrQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 10
  ).getQualificationType.wrapNullable.flatMap(_.find(_.getName == valAgrQualTypeName)).getOrElse {
    System.out.println("Generating validation qualification type...")
    config.service.createQualificationType(
      valAgrQualTypeName,
      "language,english,question answering",
      """The rate at which answers and validity judgments
       in our question answering task agreed with other validators.""".replaceAll("\\s+", " "),
      QualificationTypeStatus.Active,
      null, // retry delay (seconds)
      null, null, null, // these 3 are for a test/answer key
      true, // auto granted
      101 // auto granted value
    )
  }
  val valAgrQualTypeId = valAgrQualType.getQualificationTypeId
  val valAgreementRequirement = new QualificationRequirement(
    valAgrQualTypeId,
    Comparator.GreaterThanOrEqualTo, (math.round(validationAgreementBlockingThreshold * 100.0).toInt),
    null, false)

  val valTestQualTypeLabelString = validationTestQualTypeLabel.fold("")(x => s"[$x] ")
  val valTestQualTypeName = if(config.isProduction) s"${valTestQualTypeLabelString}Question answering test score (%)"
                            else "Sandbox test score qual"
  val valTestQualType = config.service.searchQualificationTypes(
    valTestQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 10
  ).getQualificationType.wrapNullable.flatMap(_.find(_.getName == valTestQualTypeName)).getOrElse {
    System.out.println("Generating validation test qualification type...")
    config.service.createQualificationType(
      valTestQualTypeName,
      "language,english,question answering",
      """Score on the qualification test for the question answering task,
         as a test of your understanding of the instructions.""".replaceAll("\\s+", " "),
      QualificationTypeStatus.Active,
      300L, // retry delay (seconds) --- 5 minutes
      qualTest.testString, // test: QuestionForm
      qualTest.answerKeyString, // AnswerKey
      1200L, // test time limit (seconds) --- 30 minutes
      false, // auto granted
      null // auto granted value
    )
  }
  val valTestQualTypeId = valTestQualType.getQualificationTypeId
  val valTestRequirement = new QualificationRequirement(
    valTestQualTypeId,
    Comparator.GreaterThanOrEqualTo, 75,
    null, false)

  val genHITType = HITType(
    title = s"Write question-answer pairs about a sentence's meaning",
    description = s"""
      Given a sentence and some words from that sentence,
      write questions and answers involving each word.
      Write more question-answer pairs for increasing bonuses, and
      maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
    reward = generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, genAccuracyRequirement
    ))

  lazy val genApiFlow = Flow[GenerationApiRequest[SID]].map {
    case GenerationApiRequest(id) =>
      GenerationApiResponse(id.tokens)
  }

  lazy val sampleGenPrompt = GenerationPrompt[SID](allIds.head, tokenSplits(allIds.head.tokens).head)

  lazy val genTaskSpec = TaskSpecification[GenerationPrompt[SID], List[WordedQAPair], GenerationApiRequest[SID], GenerationApiResponse](
    expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt,
    frozenHITTypeId = frozenGenerationHITTypeID)

  // validation task definition

  val valHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and several questions about it,
      highlight the part of the sentence that answers each question,
      and mark questions that are invalid or redundant.
      Maintain high agreement with others to stay qualified.
    """.trim,
    reward = validationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, valAgreementRequirement, valTestRequirement
    ))

  lazy val valApiFlow = Flow[ValidationApiRequest[SID]].map {
    case ValidationApiRequest(id) =>
      ValidationApiResponse(id.tokens)
  }

  lazy val sampleValPrompt = ValidationPrompt[SID](
    sampleGenPrompt, "", "",
    List(WordedQAPair(0, "Who is awesome?", Set(1, 2, 3, 4)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val valTaskSpec = TaskSpecification[ValidationPrompt[SID], List[ValidationAnswer], ValidationApiRequest[SID], ValidationApiResponse](
    expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt,
    frozenHITTypeId = frozenValidationHITTypeID)

  // hit management --- circularly defined so they can communicate

  lazy val allPrompts = allIds.flatMap { id =>
    val tokens = id.tokens
    val splits = tokenSplits(tokens)
    splits.map(GenerationPrompt[SID](id, _))
  }

  import config.actorSystem

  var sentenceTrackerPeek: SentenceTracker[SID] = null

  lazy val sentenceTracker: ActorRef = actorSystem.actorOf(
    Props {
      sentenceTrackerPeek = new SentenceTracker[SID](genTaskSpec.hitTypeId, valTaskSpec.hitTypeId)
      sentenceTrackerPeek
    })

  var genManagerPeek: GenerationHITManager[SID] = null
  var valManagerPeek: ValidationHITManager[SID] = null

  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props {
        genManagerPeek = new GenerationHITManager(
          genHelper,
          genAccQualTypeId,
          valHelper,
          valManager,
          sentenceTracker,
          numGenerationAssignmentsForPrompt, 30, allPrompts.iterator)
        genManagerPeek
      })
  } else {
    actorSystem.actorOf(
      Props {
        genManagerPeek = new GenerationHITManager(
          genHelper,
          genAccQualTypeId,
          valHelper,
          valManager,
          sentenceTracker,
          _ => 1, 3, allPrompts.iterator)
        genManagerPeek
      })
  }

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props {
        valManagerPeek = ValidationHITManager(
          valHelper,
          valAgrQualTypeId,
          genManager,
          sentenceTracker,
          _ => 2, 50)
        valManagerPeek
      })
  } else {
    actorSystem.actorOf(
      Props {
        valManagerPeek = ValidationHITManager(
          valHelper,
          valAgrQualTypeId,
          genManager,
          sentenceTracker,
          _ => 1, 3)
        valManagerPeek
      })
  }

  val dashboardApiFlow = Flow[Unit]
    .merge(Source.tick(initialDelay = 0.seconds, interval = 1.minute, ()))
    .filter(_ => genManagerPeek != null && valManagerPeek != null && sentenceTrackerPeek != null)
    .map { _ =>
    val last5Sentences = sentenceTrackerPeek.finishedSentenceStats.take(5).flatMap { stats =>
      val sentence = stats.id.tokens
      scala.util.Try(
        stats -> SentenceHITInfo(
          sentence,
          stats.genHITIds.toList
            .map(hitDataService.getHITInfo[GenerationPrompt[SID], List[WordedQAPair]](genTaskSpec.hitTypeId, _))
            .map(_.get),
          stats.valHITIds.toList
            .map(hitDataService.getHITInfo[ValidationPrompt[SID], List[ValidationAnswer]](valTaskSpec.hitTypeId, _))
            .map(_.get))
      ).toOption
    }.toMap
    SummaryInfo(
      // generation
      numGenActive = genHelper.numActiveHITs,
      genWorkerStats = genManagerPeek.allWorkerStats,
      genFeedback = genManagerPeek.feedbacks.take(20),
      // validation
      numValPromptsWaiting = valManagerPeek.queuedPrompts.numManuallyEnqueued,
      numValActive = valHelper.numActiveHITs,
      valWorkerInfo = valManagerPeek.allWorkerInfo,
      valFeedback = valManagerPeek.feedbacks.take(20),
      // final results
      lastFewSentences = last5Sentences,
      aggSentenceStats = sentenceTrackerPeek.aggregateSentenceStats)
  }

  lazy val dashboardTaskSpec = TaskSpecification[Unit, Unit, Unit, SummaryInfo[SID]](
    expHDashboardTaskKey, null, dashboardApiFlow, (),
    frozenHITTypeId = null)

  lazy val server = new Server(List(genTaskSpec, valTaskSpec, dashboardTaskSpec))
  lazy val genActor = actorSystem.actorOf(Props(new TaskManager(genHelper, genManager)))
  lazy val valActor = actorSystem.actorOf(Props(new TaskManager(valHelper, valManager)))

  // used to schedule data-saves
  private[this] var schedule: List[Cancellable] = Nil
  def startSaves(interval: FiniteDuration = 5 minutes): Unit = {
    if(schedule.exists(_.isCancelled) || schedule.isEmpty) {
      schedule = List(genManager, valManager, sentenceTracker).map(actor =>
        config.actorSystem.scheduler.schedule(
          2 seconds, interval, actor, SaveData)(
          config.actorSystem.dispatcher, actor)
      )
    }
  }
  def stopSaves = schedule.foreach(_.cancel())
  def saveData = {
    genManager ! SaveData
    valManager ! SaveData
    sentenceTracker ! SaveData
  }

  def setGenHITsActive(n: Int) =
    genManager ! SetNumHITsActive(n)
  def setValHITsActive(n: Int) =
    valManager ! SetNumHITsActive(n)

  import TaskManager._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    startSaves()
    genActor ! Start(interval, delay = 0 seconds)
    valActor ! Start(interval, delay = 3 seconds)
  }
  def stop() = {
    genActor ! Stop
    valActor ! Stop
    stopSaves
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
  def save() = {
    sentenceTracker ! SaveData
    genManager ! SaveData
    valManager ! SaveData
  }

  // for use while it's running. Ideally instead of having to futz around at the console calling these functions,
  // in the future you could have a nice dashboard UI that will help you examine common sources of issues

  def allGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[SID], List[WordedQAPair]](genTaskSpec.hitTypeId).get
  def allValInfos = hitDataService.getAllHITInfo[ValidationPrompt[SID], List[ValidationAnswer]](valTaskSpec.hitTypeId).get

  def workerGenInfos(workerId: String) = for {
    hi <- allGenInfos
    assignment <- hi.assignments
    if assignment.workerId == workerId
  } yield HITInfo(hi.hit, List(assignment))

  // sorted increasing by number of disagreements
  def workerValInfos(workerId: String) = {
    val scored = for {
      hi <- allValInfos
      if hi.assignments.exists(_.workerId == workerId)
      workerAssignment = hi.assignments.find(_.workerId == workerId).get
      nonWorkerAssignments = hi.assignments.filter(_.workerId != workerId)
      avgNumDisagreed = hi.hit.prompt.qaPairs.size - nonWorkerAssignments.map(a => ValidationAnswer.numAgreed(workerAssignment.response, a.response)).mean
    } yield (HITInfo(hi.hit, workerAssignment :: nonWorkerAssignments), avgNumDisagreed)
    scored.sortBy(_._2).map(_._1)
  }

  def currentGenSentences: List[(SID, String)] = {
    genHelper.activeHITInfosByPromptIterator.map(_._1.id).map(id =>
      id -> Text.render(id.tokens)
    ).toList
  }

  def renderValidation(info: HITInfo[ValidationPrompt[SID], List[ValidationAnswer]]) = {
    val sentence = info.hit.prompt.genPrompt.id.tokens
    info.assignments.map { assignment =>
      Text.render(sentence) + "\n" +
        info.hit.prompt.qaPairs.zip(assignment.response).map {
          case (WordedQAPair(kwIndex, question, answerIndices), valAnswer) =>
            val answerString = Text.renderSpan(sentence, answerIndices)
            val validationString = ValidationAnswer.render(sentence, valAnswer, info.hit.prompt.qaPairs)
            s"\t$question --> $answerString \t|$validationString"
        }.mkString("\n")
    }.mkString("\n") + "\n"
  }

  def allSentenceStats: Map[SID, SentenceStats[SID]] = {
    val genInfosById = allGenInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosById = allValInfos.groupBy(_.hit.prompt.genPrompt.id).withDefaultValue(Nil)
    allIds.map { id =>
      val afterGen = genInfosById(id)
        .map(_.hit.prompt.keywords.toSet)
        .foldLeft(emptyStatus(id))(_ withKeywords _)
      val valStart = valInfosById(id)
        .map(_.hit.prompt)
        .foldLeft(afterGen)(_ beginValidation _)
      val valFinish = valInfosById(id)
        .foldLeft(valStart) {
        case (status, hitInfo) => status.finishValidation(hitInfo.hit.prompt, hitInfo.assignments)
      }
      id -> makeStats(valFinish, genTaskSpec.hitTypeId, valTaskSpec.hitTypeId)
    }.toMap
  }

  def aggSentenceStats: AggregateSentenceStats = AggregateSentenceStats.aggregate(
    allSentenceStats.values.toList
  )
}
