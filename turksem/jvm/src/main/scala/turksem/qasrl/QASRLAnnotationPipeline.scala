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
  generationAccuracyQualTypeLabel: Option[String] = None,
  validationAgreementQualTypeLabel: Option[String] = None)(
  implicit config: TaskConfig,
  inflections: Inflections) {

  implicit object SIDHasKeyIndices extends HasKeyIndices[SID] {
    override def getKeyIndices(id: SID): Set[Int] = {
      val posTaggedTokens = PosTagger.posTag(id.tokens)
      posTaggedTokens.collect {
        case Word(index, pos, token) if PosTags.verbPosTags.contains(pos) =>
          if( // detect if "have"-verb is an auxiliary
            Inflections.haveVerbs.contains(token.lowerCase) &&
              (posTaggedTokens.lift(index + 1).map(_.token.lowerCase).nonEmptyAnd(Inflections.negationWords.contains) || // negation appears directly after, or
                 posTaggedTokens.drop(index + 1).forall(_.pos != "VBN") || // there is no past-participle verb afterward, or
                 posTaggedTokens.drop(index + 1) // after the "have" verb,
                 .takeWhile(_.pos != "VBN") // until the next past-participle form verb,
                 .forall(w => Inflections.negationWords.contains(w.token.lowerCase) || PosTags.adverbPosTags.contains(w.pos)) // everything is an adverb or negation (though I guess negs are RB)
              )
          ) None else if( // detect if "do"-verb is an auxiliary
            Inflections.doVerbs.contains(token.lowerCase) &&
              (posTaggedTokens.lift(index + 1).map(_.token.lowerCase).nonEmptyAnd(Inflections.negationWords.contains) || // negation appears directly after, or
                 posTaggedTokens.drop(index + 1).forall(w => w.pos != "VB" && w.pos != "VBP") || // there is no stem or non-3rd-person present verb afterward (to mitigate pos tagger mistakes), or
                 posTaggedTokens.drop(index + 1) // after the "do" verb,
                 .takeWhile(w => w.pos != "VBP" && w.pos != "VBP") // until the next VB or VBP verb,
                 .forall(w => Inflections.negationWords.contains(w.token.lowerCase) || PosTags.adverbPosTags.contains(w.pos)) // everything is an adverb or negation (though I guess negs are RB)
              )
          ) None else inflections.getInflectedForms(token.lowerCase).map(_ => index)
      }.flatten.toSet
    }
  }

  lazy val allPrompts: Vector[GenerationPrompt[SID]] = for {
    id <- allIds
    verbIndex <- id.keyIndices.toList.sorted
  } yield GenerationPrompt(id, List(verbIndex))

  // lazy val (smallPrompts, largePrompts) = allPrompts.partition(_.keywords.size < 3)

  implicit val ads = annotationDataService

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
    Comparator.GreaterThanOrEqualTo, (math.round(QASRLSettings.generationAccuracyBlockingThreshold * 100.0).toInt),
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
    Comparator.GreaterThanOrEqualTo, (math.round(QASRLSettings.validationAgreementBlockingThreshold * 100.0).toInt),
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

  val genHITType = HITType(
    title = s"Write question-answer pairs about a verb",
    description = s"""
      Given a sentence and a verb from that sentence,
      write questions and answers about that verb.
      Questions must adhere to a certain template,
      provided by autocomplete functionality.
      Maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
    reward = QASRLSettings.generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, genAccuracyRequirement // TODO qual task req
    ))

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
    reward = QASRLSettings.validationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, valAgreementRequirement // TODO maybe another requirement
    ))

  lazy val valApiFlow = Flow[ValidationApiRequest[SID]].map {
    case ValidationApiRequest(id) =>
      ValidationApiResponse(id.tokens)
  }

  lazy val sampleValPrompt = QASRLValidationPrompt[SID](
    sampleGenPrompt, "", "", "",
    List(VerbQA(0, "Who did someone look at?", List(Set(4))),
         VerbQA(1, "Who looked at someone?", List(Set(0, 1))),
         VerbQA(1, "How did someone look at someone?", List(Set(5)))))

  lazy val valTaskSpec = TaskSpecification[QASRLValidationPrompt[SID], List[QASRLValidationAnswer], ValidationApiRequest[SID], ValidationApiResponse](
    QASRLSettings.validationTaskKey, valHITType, valApiFlow, sampleValPrompt,
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

  var genManagerPeek: QASRLGenerationHITManager[SID] = null

  def makeGenHITManagement(hitType: HITType, prompts: Vector[GenerationPrompt[SID]], setPeek: (QASRLGenerationHITManager[SID] => Unit)) = {
    val taskSpec = TaskSpecification[GenerationPrompt[SID], List[VerbQA], QASRLGenerationApiRequest[SID], QASRLGenerationApiResponse](
      QASRLSettings.generationTaskKey, hitType, genApiFlow, sampleGenPrompt,
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

  lazy val (genTaskSpec, genHelper, genManager, genActor) = makeGenHITManagement(
    genHITType, allPrompts, genManagerPeek = _)

  lazy val server = new Server(List(genTaskSpec, valTaskSpec))

  // used to schedule data-saves
  private[this] var schedule: List[Cancellable] = Nil
  def startSaves(interval: FiniteDuration = 5 minutes): Unit = {
    if(schedule.exists(_.isCancelled) || schedule.isEmpty) {
      schedule = List(genManager, valManager, accuracyTracker).map(actor =>
        config.actorSystem.scheduler.schedule(
          2 seconds, interval, actor, SaveData)(
          config.actorSystem.dispatcher, actor)
      )
    }
  }
  def stopSaves = schedule.foreach(_.cancel())

  def setGenHITsActiveEach(n: Int) = {
    genManager ! SetNumHITsActive(n)
  }
  def setValHITsActive(n: Int) = {
    valManager ! SetNumHITsActive(n)
  }

  import TaskManager.Message._
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
    // sentenceTracker ! SaveData
    accuracyTracker ! SaveData
    genManager ! SaveData
    valManager ! SaveData
  }

  // for use while it's running. Ideally instead of having to futz around at the console calling these functions,
  // in the future you could have a nice dashboard UI that will help you examine common sources of issues

  def allGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[SID], List[VerbQA]](genTaskSpec.hitTypeId).get

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
    genHelper.activeHITInfosByPromptIterator.map(_._1.id).map(id =>
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

  def printGenFeedback(n: Int) = genManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )
  def printValFeedback(n: Int) = valManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )

  def printAllFeedbacks(n: Int = Int.MaxValue) = {
    println("Generation:")
    printGenFeedback(n)
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
