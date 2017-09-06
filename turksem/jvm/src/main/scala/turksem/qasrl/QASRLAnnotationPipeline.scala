package turksem.qasrl

import cats.implicits._

import akka.actor._
import akka.stream.scaladsl.{Flow, Source}
import com.amazonaws.mturk.requester._
import com.amazonaws.mturk.service.axis.RequesterService

import nlpdata.structure._
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text
import nlpdata.util.PosTags
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.Inflections

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr._
import turksem.HasKeyIndices.ops._

import upickle.default._

import scala.concurrent.duration._
import scala.language.postfixOps

class QASRLAnnotationPipeline[SID : Reader : Writer : HasTokens](
  val allIds: Vector[SID], // IDs of sentences to annotate
  numGenerationAssignmentsForPrompt: GenerationPrompt[SID] => Int,
  annotationDataService: AnnotationDataService,
  frozenSmallGenerationHITTypeID: Option[String] = None,
  frozenLargeGenerationHITTypeID: Option[String] = None,
  frozenValidationHITTypeID: Option[String] = None,
  generationAccuracyQualTypeLabel: Option[String] = None,
  validationAgreementQualTypeLabel: Option[String] = None)(
  implicit config: TaskConfig,
  inflections: Inflections) {

  implicit object SIDHasKeyIndices extends HasKeyIndices[SID] {
    override def getKeyIndices(id: SID): Set[Int] = {
      val posTaggedTokens = PosTagger.posTag(id.tokens)
      posTaggedTokens.collect {
        case Word(index, pos, token) if PosTags.verbPosTags.contains(pos) =>
          // detect if "have"-verb is an auxiliary
          if(Set("has", "have", "had").contains(token) &&
               posTaggedTokens
               .drop(index + 1) // after the "have" verb,
               .takeWhile(_.pos != "VBN") // until the next past-participle form verb,
               .forall(w => PosTags.adverbPosTags.contains(w.pos)) // everything is an adverb
          ) None else inflections.getInflectedForms(token.lowerCase).map(_ => index)
      }.flatten.toSet
    }
  }

  lazy val allPrompts: Vector[GenerationPrompt[SID]] = for {
    id <- allIds
    verbIndices = id.keyIndices.toList.sorted
    if verbIndices.nonEmpty
  } yield GenerationPrompt(id, verbIndices)

  lazy val (smallPrompts, largePrompts) = allPrompts.partition(_.keywords.size < 3)

  implicit val ads = annotationDataService
  implicit val settings = QASRLSettings

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

  val smallGenHITType = HITType(
    title = s"Write question-answer pairs about verbs (1-2 verbs)",
    description = s"""
      Given a sentence and some verbs from that sentence,
      write questions about each verb and their answers.
      Questions must adhere to a certain template,
      provided by autocomplete functionality.
      Maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
    reward = QASRLSettings.smallGenerationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, genAccuracyRequirement
    ))

  val largeGenHITType = smallGenHITType.copy(
    title = s"Write question-answer pairs about verbs (3 or more verbs)",
    reward = QASRLSettings.largeGenerationReward
  )

  lazy val genApiFlow = Flow[QASRLGenerationApiRequest[SID]].map {
    case QASRLGenerationApiRequest(GenerationPrompt(id, keywords)) =>
      val tokens = id.tokens
      val indicesWithInflectedForms = keywords.flatMap(kw =>
        inflections.getInflectedForms(tokens(kw).lowerCase).map(forms => IndexWithInflectedForms(kw, forms))
      )
      QASRLGenerationApiResponse(tokens, indicesWithInflectedForms)
  }

  lazy val sampleGenPrompt = allPrompts.head

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
      approvalRateRequirement, locationRequirement, valAgreementRequirement
    ))

  lazy val valApiFlow = Flow[ValidationApiRequest[SID]].map {
    case ValidationApiRequest(id) =>
      ValidationApiResponse(id.tokens)
  }

  lazy val sampleValPrompt = QASRLValidationPrompt[SID](
    sampleGenPrompt, "", "", "",
    List(VerbQA(0, "Who is awesome?", List(Set(1, 2, 3, 4))),
         VerbQA(1, "What is Julian?", List(Set(5, 6, 8, 9))),
         VerbQA(1, "Whih one is best?", List(Set(5, 6, 8, 9))),
         VerbQA(1, "Herp derp?", List(Set(5, 6, 8, 9)))))

  lazy val valTaskSpec = TaskSpecification[QASRLValidationPrompt[SID], List[QASRLValidationAnswer], ValidationApiRequest[SID], ValidationApiResponse](
    validationTaskKey, valHITType, valApiFlow, sampleValPrompt,
    frozenHITTypeId = frozenValidationHITTypeID,
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks)

  // val dashboardApiFlow = Flow[Unit]
  //   .merge(Source.tick(initialDelay = 0.seconds, interval = 1.minute, ()))
  //   .filter(_ => smallGenManagerPeek != null &&
  //             largeGenManagerPeek != null &&
  //             valManagerPeek != null &&
  //             sentenceTrackerPeek != null)
  //   .map { _ =>
  //   val last5Sentences = sentenceTrackerPeek.finishedSentenceStats.take(5).flatMap { stats =>
  //     val sentence = stats.id.tokens
  //     scala.util.Try(
  //       stats -> SentenceHITInfo(
  //         sentence,
  //         stats.genHITIds.toList
  //           .map(hitDataService.getHITInfo[GenerationPrompt[SID], List[VerbQA]](genTaskSpec.hitTypeId, _))
  //           .map(_.get),
  //         stats.valHITIds.toList
  //           .map(hitDataService.getHITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](valTaskSpec.hitTypeId, _))
  //           .map(_.get))
  //     ).toOption
  //   }.toMap
  //   SummaryInfo(
  //     // generation
  //     numGenActive = smallGenHelper.numActiveHITs + largeGenHelper.numActiveHITs,
  //     genWorkerStats = genManagerPeek.allWorkerStats,
  //     genFeedback = (smallGenManagerPeek.feedbacks.take(10) ++ largeGenManagerPeek.feedbacks.take(10)),
  //     // validation
  //     numValPromptsWaiting = valManagerPeek.queuedPrompts.numManuallyEnqueued,
  //     numValActive = valHelper.numActiveHITs,
  //     valWorkerInfo = valManagerPeek.allWorkerInfo,
  //     valFeedback = valManagerPeek.feedbacks.take(20),
  //     // final results
  //     lastFewSentences = last5Sentences,
  //     aggSentenceStats = sentenceTrackerPeek.aggregateSentenceStats)
  // }

  // lazy val dashboardTaskSpec = TaskSpecification[Unit, Unit, Unit, SummaryInfo[SID]](
  //   dashboardTaskKey, null, dashboardApiFlow, (),
  //   frozenHITTypeId = null)

  // hit management --- circularly defined so they can communicate

  import config.actorSystem

  var accuracyTrackerPeek: QASRLGenerationAccuracyManager[SID] = null

  lazy val accuracyTracker: ActorRef = actorSystem.actorOf(
    Props {
      accuracyTrackerPeek = new QASRLGenerationAccuracyManager[SID](genAccQualTypeId)
      accuracyTrackerPeek
    }
  )

  // var sentenceTrackerPeek: QASRLSentenceTracker[SID] = null

  // lazy val sentenceTracker: ActorRef = actorSystem.actorOf(
  //   Props {
  //     sentenceTrackerPeek = new QASRLSentenceTracker[SID](valTaskSpec.hitTypeId)
  //     sentenceTrackerPeek
  //   })

  var valManagerPeek: QASRLValidationHITManager[SID] = null

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props {
        valManagerPeek = QASRLValidationHITManager(
          valHelper,
          valAgrQualTypeId,
          accuracyTracker,
          // sentenceTracker,
          _ => 2, 50)
        valManagerPeek
      })
  } else {
    actorSystem.actorOf(
      Props {
        valManagerPeek = QASRLValidationHITManager(
          valHelper,
          valAgrQualTypeId,
          accuracyTracker,
          // sentenceTracker,
          _ => 1, 3)
        valManagerPeek
      })
  }

  lazy val valActor = actorSystem.actorOf(Props(new TaskManager(valHelper, valManager)))

  var smallGenManagerPeek: QASRLGenerationHITManager[SID] = null
  var largeGenManagerPeek: QASRLGenerationHITManager[SID] = null

  def makeGenHITManagement(hitType: HITType, prompts: Vector[GenerationPrompt[SID]], setPeek: (QASRLGenerationHITManager[SID] => Unit), frozenHITTypeId: Option[String]) = {
    val taskSpec = TaskSpecification[GenerationPrompt[SID], List[VerbQA], QASRLGenerationApiRequest[SID], QASRLGenerationApiResponse](
      generationTaskKey, hitType, genApiFlow, sampleGenPrompt,
      frozenHITTypeId = frozenHITTypeId,
      taskPageHeadElements = taskPageHeadLinks,
      taskPageBodyElements = taskPageBodyLinks)
    val helper = new HITManager.Helper(taskSpec)
    val hitManager: ActorRef = if(config.isProduction) {
      actorSystem.actorOf(
        Props {
          val manager = new QASRLGenerationHITManager(
            helper,
            valHelper,
            valManager,
            // sentenceTracker,
            numGenerationAssignmentsForPrompt, 30, prompts.iterator)
          setPeek(manager)
          manager
        }
      )
    } else {
      actorSystem.actorOf(
        Props {
          val manager = new QASRLGenerationHITManager(
            helper,
            valHelper,
            valManager,
            // sentenceTracker,
            (_: GenerationPrompt[SID]) => 1, 3, prompts.iterator)
          setPeek(manager)
          manager
        }
      )
    }
    val actor = actorSystem.actorOf(Props(new TaskManager(helper, hitManager)))
    (taskSpec, helper, hitManager, actor)
  }

  lazy val (smallGenTaskSpec, smallGenHelper, smallGenManager, smallGenActor) = makeGenHITManagement(
    smallGenHITType, smallPrompts, smallGenManagerPeek = _, frozenSmallGenerationHITTypeID)

  lazy val (largeGenTaskSpec, largeGenHelper, largeGenManager, largeGenActor) = makeGenHITManagement(
    largeGenHITType, largePrompts, largeGenManagerPeek = _, frozenLargeGenerationHITTypeID)

  lazy val server = new Server(List(smallGenTaskSpec, largeGenTaskSpec, valTaskSpec))

  // used to schedule data-saves
  private[this] var schedule: List[Cancellable] = Nil
  def startSaves(interval: FiniteDuration = 5 minutes): Unit = {
    if(schedule.exists(_.isCancelled) || schedule.isEmpty) {
      schedule = List(smallGenManager, largeGenManager, valManager, accuracyTracker).map(actor =>
        config.actorSystem.scheduler.schedule(
          2 seconds, interval, actor, SaveData)(
          config.actorSystem.dispatcher, actor)
      )
    }
  }
  def stopSaves = schedule.foreach(_.cancel())

  def setGenHITsActiveEach(n: Int) = {
    smallGenManager ! SetNumHITsActive(n)
    largeGenManager ! SetNumHITsActive(n)
  }
  def setValHITsActive(n: Int) = {
    valManager ! SetNumHITsActive(n)
  }

  import TaskManager.Message._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    startSaves()
    smallGenActor ! Start(interval, delay = 0 seconds)
    largeGenActor ! Start(interval, delay = 0 seconds)
    valActor ! Start(interval, delay = 3 seconds)
  }
  def stop() = {
    smallGenActor ! Stop
    largeGenActor ! Stop
    valActor ! Stop
    stopSaves
  }
  def disable() = {
    smallGenActor ! Disable
    largeGenActor ! Disable
    valActor ! Disable
  }
  def expire() = {
    smallGenActor ! Expire
    largeGenActor ! Expire
    valActor ! Expire
  }
  def update() = {
    server
    smallGenActor ! Update
    largeGenActor ! Update
    valActor ! Update
  }
  def save() = {
    // sentenceTracker ! SaveData
    accuracyTracker ! SaveData
    smallGenManager ! SaveData
    largeGenManager ! SaveData
    valManager ! SaveData
  }

  // for use while it's running. Ideally instead of having to futz around at the console calling these functions,
  // in the future you could have a nice dashboard UI that will help you examine common sources of issues

  def allSmallGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[SID], List[VerbQA]](smallGenTaskSpec.hitTypeId).get
  def allLargeGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[SID], List[VerbQA]](largeGenTaskSpec.hitTypeId).get
  def allGenInfos = allSmallGenInfos ++ allLargeGenInfos

  def allValInfos = hitDataService.getAllHITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](valTaskSpec.hitTypeId).get

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
      avgNumDisagreed = hi.hit.prompt.qaPairs.size - nonWorkerAssignments.map(a => QASRLValidationAnswer.numAgreed(workerAssignment.response, a.response)).mean
    } yield (HITInfo(hi.hit, workerAssignment :: nonWorkerAssignments), avgNumDisagreed)
    scored.sortBy(_._2).map(_._1)
  }

  def currentGenSentences: List[(SID, String)] = {
    (smallGenHelper.activeHITInfosByPromptIterator ++ largeGenHelper.activeHITInfosByPromptIterator).map(_._1.id).map(id =>
      id -> Text.render(id.tokens)
    ).toList
  }

  def renderValidation(info: HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]) = {
    val sentence = info.hit.prompt.genPrompt.id.tokens
    info.assignments.map { assignment =>
      Text.render(sentence) + "\n" +
        info.hit.prompt.qaPairs.zip(assignment.response).map {
          case (VerbQA(kwIndex, question, answers), valAnswer) =>
            val answerString = answers.map(Text.renderSpan(sentence, _)).mkString(" / ")
            val validationString = QASRLValidationAnswer.render(sentence, valAnswer, info.hit.prompt.qaPairs)
            s"\t$question --> $answerString \t|$validationString"
        }.mkString("\n")
    }.mkString("\n") + "\n"
  }

  case class StatSummary(
    workerId: String,
    numQs: Option[Int],
    accuracy: Option[Double],
    numAs: Option[Int],
    pctBad: Option[Double],
    agreement: Option[Double],
    earnings: Double)

  object StatSummary {
    def makeFromStatsAndInfo(
      stats: Option[WorkerStats],
      info: Option[QASRLValidationWorkerInfo]) = stats.map(_.workerId).orElse(info.map(_.workerId)).map { wid =>
      StatSummary(
        workerId = wid,
        numQs = stats.map(_.numQAPairsWritten),
        accuracy = stats.map(_.accuracy),
        numAs = info.map(i => i.numAnswerSpans + i.numInvalids + i.numRedundants),
        pctBad = info.map(
          i => 100.0 * (i.numInvalids + i.numRedundants) / (i.numAnswerSpans + i.numInvalids + i.numRedundants)
        ),
        agreement = info.map(_.agreement),
        earnings = stats.fold(0.0)(_.earnings) + info.fold(0.0)(_.earnings)
      )
    }
  }

  def printStats[B : Ordering](sortFn: StatSummary => B) = {
    val allStats = accuracyTrackerPeek.allWorkerStats
    val allInfos = valManagerPeek.allWorkerInfo
    val summaries = (allStats.keys ++ allInfos.keys).toSet.toList.flatMap((wid: String) =>
      StatSummary.makeFromStatsAndInfo(allStats.get(wid), allInfos.get(wid))
    ).sortBy(sortFn)
    println(f"${"Worker ID"}%14s  ${"Qs"}%5s  ${"Acc"}%4s  ${"As"}%5s  ${"%Bad"}%5s  ${"Agr"}%4s  $$")
    summaries.foreach { case StatSummary(wid, numQsOpt, accOpt, numAsOpt, pctBadOpt, agrOpt, earnings)=>
      val numQs = numQsOpt.getOrElse("")
      val acc = accOpt.foldMap(pct => f"$pct%.2f")
      val numAs = numAsOpt.getOrElse("")
      val pctBad = pctBadOpt.foldMap(pct => f"$pct%.2f")
      val agr = agrOpt.foldMap(pct => f"$pct%.2f")
      println(f"$wid%14s  $numQs%5s  $acc%4s  $numAs%5s  $pctBad%4s  $agr%4s  $earnings%.2f")
    }
  }

  def printQStats = printStats(-_.numQs.getOrElse(0))
  def printAStats = printStats(-_.numAs.getOrElse(0))

  def printSmallGenFeedback(n: Int) = smallGenManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )
  def printLargeGenFeedback(n: Int) = largeGenManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )
  def printValFeedback(n: Int) = valManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )

  def printAllFeedbacks(n: Int = Int.MaxValue) = {
    println("Small gen:")
    printSmallGenFeedback(n)
    println("\nLarge gen:")
    printLargeGenFeedback(n)
    println("\nValidation:")
    printValFeedback(n)
  }

  // def allSentenceStats: Map[SID, SentenceStats[SID]] = {
  //   val genInfosById = allGenInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
  //   val valInfosById = allValInfos.groupBy(_.hit.prompt.genPrompt.id).withDefaultValue(Nil)
  //   allIds.map { id =>
  //     val afterGen = genInfosById(id)
  //       .map(_.hit.prompt.keywords.toSet)
  //       .foldLeft(emptyStatus(id))(_ withKeywords _)
  //     val valStart = valInfosById(id)
  //       .map(_.hit.prompt)
  //       .foldLeft(afterGen)(_ beginValidation _)
  //     val valFinish = valInfosById(id)
  //       .foldLeft(valStart) {
  //       case (status, hitInfo) => status.finishValidation(hitInfo.hit.prompt, hitInfo.assignments)
  //     }
  //     id -> QASRLSentenceTracker.makeStats(valFinish, genTaskSpec.hitTypeId, valTaskSpec.hitTypeId)
  //   }.toMap
  // }

  // def aggSentenceStats: AggregateSentenceStats = AggregateSentenceStats.aggregate(
  //   allSentenceStats.values.toList
  // )
}
