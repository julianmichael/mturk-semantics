package mts.experiments.expH

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.datasets.conll._
import mts.datasets.ptb._
import mts.datasets.wiki1k._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

import com.amazonaws.mturk.requester.QualificationRequirement
import com.amazonaws.mturk.requester.QualificationTypeStatus
import com.amazonaws.mturk.requester.SortDirection
import com.amazonaws.mturk.requester.SearchQualificationTypesSortProperty
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.requester.Comparator
import com.amazonaws.mturk.requester.Locale

class FinalExperiment(implicit config: TaskConfig) {
  val experimentName = finalExperimentName

  val approvalRateRequirement = new QualificationRequirement(
    RequesterService.APPROVAL_RATE_QUALIFICATION_TYPE_ID,
    Comparator.GreaterThanOrEqualTo, 95,
    null, true)

  val locationRequirement = new QualificationRequirement(
    RequesterService.LOCALE_QUALIFICATION_TYPE_ID,
    Comparator.EqualTo, null,
    new Locale("US"), true)

  val genAccQualTypeName = "Question-answer generation accuracy (%)"
  val genAccQualType = config.service.searchQualificationTypes(
    genAccQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 1
  ).getQualificationType.wrapNullable.flatMap(_.headOption).getOrElse {
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

  val valAgrQualTypeName = "Question answering agreement rate (%)"
  val valAgrQualType = config.service.searchQualificationTypes(
    valAgrQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 1
  ).getQualificationType.wrapNullable.flatMap(_.headOption).getOrElse {
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

  val valTestQualTypeName = "Question answering test score (%)"
  val valTestQualType = config.service.searchQualificationTypes(
    valTestQualTypeName, false, true, SortDirection.Ascending, SearchQualificationTypesSortProperty.Name, 1, 1
  ).getQualificationType.wrapNullable.flatMap(_.headOption).getOrElse {
    System.out.println("Generating validation test qualification type...")
    config.service.createQualificationType(
      valTestQualTypeName,
      "language,english,question answering",
      """Score on the qualification test for the question answering task,
         as a test of your understanding of the instructions.""".replaceAll("\\s+", " "),
      QualificationTypeStatus.Active,
      300L, // retry delay (seconds) --- 10 minutes
      FinalExperiment.valQualTestString, // test: QuestionForm
      FinalExperiment.valQualAnswerKeyString, // AnswerKey
      1200L, // test time limit (seconds) --- 30 minutes
      false, // auto granted
      null // auto granted value
    )
  }
  val valTestQualTypeId = valTestQualType.getQualificationTypeId
  val valTestRequirement = new QualificationRequirement(
    valTestQualTypeId,
    Comparator.GreaterThanOrEqualTo, 80,
    null, false)

  // <![CDATA[
  //      // cdata content
  //    ]]>

  // saved these manually, see code in package.scala
  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    FileManager.loadDataFile(experimentName, "origQASRLPaths.txt").get.head
  )

  val numPTB = 150

  lazy val (ptbTrain, ptbDev, ptbTest) = {
    val shuffleRand = new util.Random(832592735L)
    val (train, devTestRest) = shuffleRand.shuffle(origQASRLPaths).splitAt(numPTB * 4 / 5)
    val (dev, testRest) = devTestRest.splitAt(numPTB / 10)
    val test = testRest.take(numPTB / 10)
    (train, dev, test)
  }

  def getWikiSentences(rand: util.Random, filePaths: Vector[Wiki1kPath], numSentences: Int) = {
    rand.shuffle(
      filePaths.flatMap(p => FileManager.getWiki1kFile(p).get.paragraphs)
    ).filter(p =>
      !p.exists(sentence =>
        sentence.tokens.exists(t =>
          TextRendering.normalizeToken(t) == "\\"))
    ).flatten.map(s => s.path).take(numSentences)
  }

  val numWikipedia = 2500

  lazy val (wikipediaTrain, wikipediaDev, wikipediaTest) = {
    val shuffleRand = new util.Random(1230976L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      FileManager.wiki1kPathsForDomain("wikipedia")
    ).splitAt(640)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikipedia * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikipedia / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikipedia / 10)
    (train, dev, test)
  }

  val numWikinews = 2500

  lazy val (wikinewsTrain, wikinewsDev, wikinewsTest) = {
    val shuffleRand = new util.Random(1246902L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      FileManager.wiki1kPathsForDomain("wikinews")
        .sortBy(-_.suffix.toInt) // relies on wikinews IDs being ints... true as of now
        .take(1000)
    ).splitAt(800)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikinews * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikinews / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikinews / 10)
    (train, dev, test)
  }

  lazy val trainIds = ptbTrain.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTrain.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTrain.map(WikiSentenceId(_): SentenceId)
  lazy val trainIDSet = trainIds.toSet
  def isTrain(id: SentenceId) = trainIDSet.contains(id)

  lazy val devIds = ptbDev.map(PTBSentenceId(_): SentenceId) ++
    wikipediaDev.map(WikiSentenceId(_): SentenceId) ++
    wikinewsDev.map(WikiSentenceId(_): SentenceId)
  lazy val devIDSet = devIds.toSet
  def isDev(id: SentenceId) = devIDSet.contains(id)

  lazy val testIds = ptbTest.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTest.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTest.map(WikiSentenceId(_): SentenceId)
  lazy val testIDSet = testIds.toSet
  def isTest(id: SentenceId) = testIDSet.contains(id)

  val genHITType = HITType(
    title = s"Write question-answer pairs about a sentence's meaning",
    description = s"""
      Given a sentence and some words from that sentence,
      write questions and answers involving each word.
      Write more question-answer pairs for increasing bonuses!
    """.trim,
    reward = generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, genAccuracyRequirement
    ))

  lazy val genApiFlow = Flow[GenerationApiRequest].map {
    case GenerationApiRequest(id) =>
      GenerationApiResponse(getTokensForId(id))
  }

  lazy val sampleGenPrompt = GenerationPrompt(sourceIds(5), idSplits(sourceIds(5))(0))

  lazy val genTaskSpec = TaskSpecification[GenerationPrompt, List[WordedQAPair], GenerationApiRequest, GenerationApiResponse](
    TaskIndex.expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt)

  // validation task definition

  val valHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and several questions about it,
      highlight the part of the sentence that answers each question,
      and mark questions that are invalid or redundant.
    """.trim,
    reward = validationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, locationRequirement, valAgreementRequirement, valTestRequirement
    ))

  lazy val valApiFlow = Flow[ValidationApiRequest].map {
    case ValidationApiRequest(id) =>
      ValidationApiResponse(getTokensForId(id))
  }

  lazy val sampleValPrompt = ValidationPrompt(
    sampleGenPrompt, "", "",
    List(WordedQAPair(0, "Who is awesome?", Set(1, 2, 3, 4)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val valTaskSpec = TaskSpecification[ValidationPrompt, List[ValidationAnswer], ValidationApiRequest, ValidationApiResponse](
    TaskIndex.expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt)

  // hit management --- circularly defined so they can communicate

  lazy val sourceIds = {
    val idShuffleRand = new util.Random(218469L)
    idShuffleRand.shuffle(trainIds ++ devIds ++ testIds)
      .filter {
      case WikiSentenceId(path) =>
        !path.filePath.suffix.contains("785582") && // this is apparently a FRENCH INTERVIEW
          !path.filePath.suffix.contains("648587") // this is apparently a SPANISH ARTICLE
      case _ => true
    }
  }

  lazy val sourcePrompts = sourceIds
    .drop(860) // those done in the version with blocks
    .flatMap(id => idSplits(id).map(GenerationPrompt(id, _)))

  implicit lazy val inflections = {
    val tokens = for {
      id <- sourceIds.iterator
      word <- getTokensForId(id).iterator
    } yield word
    getInflectionsForTokens(tokens)
  }

  import config.actorSystem

  var sentenceTrackerPeek: SentenceTracker = null

  lazy val sentenceTracker: ActorRef = actorSystem.actorOf(
    Props {
      sentenceTrackerPeek = new SentenceTracker(genTaskSpec.hitTypeId, valTaskSpec.hitTypeId)
      sentenceTrackerPeek
    })

  var genManagerPeek: GenerationHITManager = null
  var valManagerPeek: ValidationHITManager = null

  def numGenerationAssignmentsForPrompt(p: GenerationPrompt) = p.id match {
    case PTBSentenceId(_) => 5
    case id @ WikiSentenceId(_) => if(isTrain(id)) 1 else 3
  }

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
          numGenerationAssignmentsForPrompt, 30, sourcePrompts.iterator)
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
          _ => 1, 3, sourcePrompts.iterator)
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
      val sentence = getTokensForId(stats.id)
      scala.util.Try(
        stats -> SentenceHITInfo(
          sentence,
          stats.genHITIds.toList
            .map(FileManager.getHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId, _))
            .map(_.get),
          stats.valHITIds.toList
            .map(FileManager.getHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId, _))
            .map(_.get))
      ).toOptionPrinting
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

  lazy val dashboardTaskSpec = TaskSpecification[Unit, Unit, Unit, SummaryInfo](
    TaskIndex.expHDashboardTaskKey, null, dashboardApiFlow, (),
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

  // convenience functions

  val oldGenHITTypeId = "3X8M0CO8US8JERH7QA0GGQIWAEHPVL"
  val oldValHITTypeId = "3OR5EJIUG2QY9PC04VUEYEGYR3Q9UL"

  def oldGenInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] =
    FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](oldGenHITTypeId)
  def oldValInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] =
    FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](oldValHITTypeId)

  def allGenInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] =
    oldGenInfos ++ FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId)
  def allValInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] = oldValInfos ++ FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId)

  def alignedInfos: Map[
    SentenceId,
    Map[GenerationPrompt,
        Map[Assignment[List[WordedQAPair]],
            List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]]]
  ] = {
    val genInfos = allGenInfos
    val valInfos = allValInfos

    sourceIds.map { id =>
      id -> sourcePrompts.filter(_.id == id).map { prompt =>
        prompt -> genInfos.filter(_.hit.prompt == prompt).flatMap(_.assignments).map { genAssignment =>
          genAssignment -> valInfos.filter(_.hit.prompt.sourceAssignmentId == genAssignment.assignmentId)
        }.toMap
      }.toMap
    }.toMap
  }

  def allSentenceStats: Map[SentenceId, SentenceStats] = {
    val genInfosById = allGenInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosById = allValInfos.groupBy(_.hit.prompt.genPrompt.id).withDefaultValue(Nil)
    sourceIds.map { id =>
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

  def renderValidation(info: HITInfo[ValidationPrompt, List[ValidationAnswer]]) = {
    val sentence = getTokensForId(info.hit.prompt.genPrompt.id)
    info.assignments.map { assignment =>
      TextRendering.renderSentence(sentence) + "\n" +
        info.hit.prompt.qaPairs.zip(assignment.response).map {
          case (WordedQAPair(kwIndex, question, answerIndices), valAnswer) =>
            val answerString = TextRendering.renderSpan(sentence, answerIndices)
            val validationString = renderValidationAnswer(sentence, valAnswer, info.hit.prompt.qaPairs)
            s"\t$question --> $answerString \t|$validationString"
        }.mkString("\n")
    }.mkString("\n") + "\n"
  }

  // TODO make hierarchical list and use a CSV reader; at least, do that when desire to share data.
  def makeTSV(
    ids: List[SentenceId],
    genInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]
  ): String = {
    val sb = new StringBuilder
    import scalaz._
    import Scalaz._
    import scala.language.higherKinds
    type PrintingState[A] = State[List[String], A]
    type Printing[A] = ListT[PrintingState, A]
    def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
    def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))
    val processor = for {
      id <- iter(ids)
      sentence = getTokensForId(id)
      _ <- append("\t\t\t" + sentence.mkString(" ") + "\n")
      (genWorkerId, WordedQAPair(keywordIndex, question, answerIndices), valAnswers, valAnswersString, valFeedback) <- iter {
        val qaPairs = for {
          HITInfo(genHIT, genAssignments) <- genInfos
          if genHIT.prompt.id == id
          genAssignment <- genAssignments
          chosenValInfo <- valInfos.find(_.hit.prompt.sourceAssignmentId.equals(genAssignment.assignmentId)).toList
          (wqa, qaIndex) <- genAssignment.response.zipWithIndex
          valAnswers = chosenValInfo.assignments.map(a => a.response(qaIndex))
          valFeedback = chosenValInfo.assignments.map(a => a.feedback).filterNot(_.isEmpty)
        } yield (
          genAssignment.workerId,
          wqa,
          valAnswers,
          valAnswers.map(valAnswer =>
            renderValidationAnswer(sentence, valAnswer, genAssignment.response)
          ).mkString("\t"),
          valFeedback.mkString("\t")
        )
        qaPairs.sortBy(_._2.wordIndex)
      }
      questionIndices = getWordsInQuestion(sentence, question).mkString(" ")
      _ <- append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${genWorkerId}\t")
      _ <- append(s"${sentence(keywordIndex)} ($keywordIndex)\t$questionIndices\t$question\t")
      _ <- append(TextRendering.renderSpan(sentence, answerIndices) + s"\t$valAnswersString\t")
      _ <- append(s"${answerIndices.mkString(" ")}\t")
      _ <- append(valAnswers.map(_.getAnswer.map(_.indices.mkString(" ")).getOrElse("")).mkString("\t"))
      _ <- append(s"\t$valFeedback")
      _ <- append("\n")
    } yield ()
    processor.run.exec(Nil).reverse.mkString
  }

  def writeTSVs(
    genInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] = allGenInfos,
    valInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] = allValInfos
  ) = {
    val allIds = genInfos.map(_.hit.prompt.id).toSet.toList
    val trainIds = allIds.filter(isTrain)
    val devIds = allIds.filter(isDev)
    val testIds = allIds.filter(isTest)
    FileManager.saveDataFile(experimentName, "train.tsv", makeTSV(trainIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "dev.tsv", makeTSV(devIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "test.tsv", makeTSV(testIds, genInfos, valInfos))
  }

  lazy val assignmentCosts = allValInfos.map {
    case HITInfo(hit, assignments) =>
      val numSpecialWords = hit.prompt.genPrompt.keywords.size
      val numQAsProvided = hit.prompt.qaPairs.size
      val numQAsValid = math.round(assignments.map(_.response.filter(_.isAnswer).size).mean - 0.01).toInt
      val genBonus = (1 to (numQAsValid - numSpecialWords)).map(bonusFor).sum
      val valReward = 0.15 + (0.03 * math.max(0, numQAsProvided - 4))
      0.20 + genBonus + (2 * valReward)
  }

  def assignmentNumQAs = allValInfos.map {
    case HITInfo(hit, assignments) => hit.prompt.qaPairs.size
  }

  def assignmentNumValids = allValInfos.map {
    case HITInfo(hit, assignments) =>
      math.round(assignments.map(_.response.filter(_.isAnswer).size).mean - 0.01).toInt
  }

  def assignmentNumRedundants = allValInfos.map {
    case HITInfo(hit, assignments) =>
      math.round(assignments.map(_.response.filter(_.isRedundant).size).mean).toInt
  }

  def numBothAnswered = allValInfos.map(
    _.assignments
      .map(a => a.response.map(resolveRedundancy(_, a.response)))
      .transpose
      .filter(_.forall(_.isAnswer))
      .size
  )

  def numBothAnsweredAndNoIntersection = allValInfos.map(
    _.assignments
      .map(a => a.response.map(resolveRedundancy(_, a.response)))
      .transpose
      .filter(_.forall(_.isAnswer))
      .filter(_.flatMap(_.getAnswer).map(_.indices).reduce(_ intersect _).isEmpty)
      .size
  ).sum

  def numAgreements = allValInfos.map(hi => numAgreed(hi.assignments(0), hi.assignments(1)))

  def numQAPairsEachSpecialWordInHIT = for {
    HITInfo(hit, assignments) <- allGenInfos
    sentence = getTokensForId(hit.prompt.id)
    assignment <- assignments
    keyword <- hit.prompt.keywords
  } yield assignment.response
    .map(wqa => getWordsInQuestion(sentence, wqa.question) ++ wqa.answer)
    .filter(_.contains(keyword)).size

  def assignmentsMissingSpecialWords = for {
    HITInfo(hit, assignments) <- allGenInfos
    sentence = getTokensForId(hit.prompt.id)
    assignment <- assignments
    keyword <- hit.prompt.keywords
    if assignment.response
    .map(wqa => getWordsInQuestion(sentence, wqa.question) ++ wqa.answer)
    .filter(_.contains(keyword)).isEmpty
  } yield (sentence, TextRendering.renderSentence(sentence), keyword,
           TextRendering.normalizeToken(sentence(keyword)),
           assignment.response)

  def orderedPairsCovered(sentence: Vector[String], question: String, answerIndices: Set[Int]) = {
    val pairs = for {
      qi <- getWordsInQuestion(sentence, question)
      ai <- answerIndices
    } yield (math.min(qi, ai), math.max(qi, ai))
    pairs.toSet
  }

  def sampleQAPairs(id: SentenceId, n: Int = 1) = {
    val promptToAlignments = alignedInfos(id)
    val sentence = getTokensForId(id)
    val qas = promptToAlignments.values.flatMap { alignment =>
      // sample n assignments
      val sample = util.Random.shuffle(alignment.keys.toVector).take(n)
      for {
        genAssignment <- sample
        valResponses = alignment(genAssignment).flatMap(_.assignments).map(_.response)
        (wqa, index) <- genAssignment.response.zipWithIndex
        qResponses = valResponses.map(_(index))
        if qResponses.forall(!_.isInvalid)
        allValIndices = qResponses.flatMap(_.getAnswer).map(_.indices).foldLeft(Set.empty[Int])(_ union _)
      } yield WordedQAPair(wqa.wordIndex, wqa.question, allValIndices ++ wqa.answer)
    }
    qas
  }

  case class PairCoverage(
    id: SentenceId,
    pairs: Set[(Int, Int)]) {
    def sentence = getTokensForId(id)
    def numCovered = pairs.size
    def numPossible = math.pow(sentence.size, 2).toInt
  }
  def nSamplePairCoverage(n: Int): Map[SentenceId, PairCoverage] = alignedInfos.map {
    case (id, promptToAlignments) =>
      val sentence = getTokensForId(id)
      val wqas = sampleQAPairs(id, n)
      val pairs = wqas.map {
        case WordedQAPair(_, question, answer) => orderedPairsCovered(sentence, question, answer)
      }.foldLeft(Set.empty[(Int, Int)])(_ union _)
      id -> PairCoverage(id, pairs)
  }.toMap

  def coverageCountsBySentence = {
    val coverages = (1 to 5).map(nSamplePairCoverage).toList
    sourceIds.map { id =>
      id -> coverages.map(_(id)).map(_.numCovered)
    }.toMap
  }

  def avgCoveragePercentages(n: Int = 20) = {
    coverageCountsBySentence.values.toVector
      .sortBy(_.last)
      .take(n)
      .map(counts => counts.map(_ * 100.0 / counts.last))
      .toList.transpose.map(_.mean)
  }

  def avgQAsPerKeywordByWorker = allGenInfos.flatMap(_.assignments).groupBy(_.workerId).map {
    case (worker, as) => worker -> {
      val nums = as.flatMap(_.response).groupBy(_.wordIndex).map(_._2.size).toList
      (nums.mean, nums.size)
    }
  }

  def alignToPAS(words: Vector[String], qas: List[WordedQAPair], paStructures: List[PredicateArgumentStructure]) = {
    case class PredArg(pred: Predicate, arg: ArgumentSpan)
    import mts.analysis
    val predArgs = paStructures
      .flatMap(pas => pas.arguments.map(PredArg(pas.pred, _)))
      .filterNot(pa => analysis.labelIsIrrelevant(pa.arg.label))
      .filterNot(pa => analysis.copulas.contains(pa.pred.head.token.toLowerCase))
      .filterNot(pa => pa.arg.words.contains(pa.pred.head))
    val alignedPAs = qas.map {
      case WordedQAPair(_, question, answer) =>
        val qWords = getWordsInQuestion(words, question)
        def alignment(predSide: Set[Int], argSide: Set[Int]) = predArgs
          .filter(pa => predSide.contains(pa.pred.head.index))
          .map(pa => pa -> argSide.intersect(pa.arg.words.map(_.index).toSet).size.toDouble / argSide.union(pa.arg.words.map(_.index).toSet).size)
          .filter(_._2 > 0)
          .sortBy(-_._2)
          .headOption
        val bestAlignment = List(
          alignment(qWords, answer),
          alignment(answer, qWords)
        ).flatten.sortBy(-_._2).map(_._1).headOption
        bestAlignment
    }
    // collapse QAs redundantly aligned... TODO: try without collapsing
    val pasCovered = alignedPAs.flatten.toSet
    val numPredictions = alignedPAs.filter(_ == None).size + pasCovered.size

    val missedDeps = predArgs.filterNot(pasCovered).mkString("\n")
    println(s"Missed deps:\n$missedDeps")

    PrecisionRecall(
      numPredicted = numPredictions,
      numGold = predArgs.size,
      numCorrect = pasCovered.size,
      numCovered = pasCovered.size
    )
  }

  def propBankPR(path: PTBSentencePath, qas: List[WordedQAPair]): PrecisionRecall = {
    import mts.datasets.propbank._
    val pbPath = ptbToPropBankSentencePath(path)
    pbPath
      .flatMap((p => FileManager.getPropBankSentence(p)) andThen (_.toOption))
      .fold(PrecisionRecall(qas.size, 0, 0, 0)) { pbSentence =>
      val paStructures = pbSentence.predicateArgumentStructures
      alignToPAS(getTokensForId(PTBSentenceId(path)), qas, paStructures)
    }
  }

  def allPropBankPRs(n: Int = 1) = alignedInfos.collect {
    case (id @ PTBSentenceId(path), promptToAlignments) =>
      val sentence = getTokensForId(id)
      val qas = sampleQAPairs(id, n)
      propBankPR(path, qas.toList)
  }.toList

  def nomBankPR(path: PTBSentencePath, qas: List[WordedQAPair]): PrecisionRecall = {
    import mts.datasets.nombank._
    FileManager.getNomBankPredArgStructuresReindexed(path).toOption
      .fold(PrecisionRecall(qas.size, 0, 0, 0)) { paStructures =>
      alignToPAS(getTokensForId(PTBSentenceId(path)), qas, paStructures)
    }
  }

  def allNomBankPRs(n: Int = 1) = alignedInfos.collect {
    case (id @ PTBSentenceId(path), promptToAlignments) =>
      val sentence = getTokensForId(id)
      val qas = sampleQAPairs(id, n)
      nomBankPR(path, qas.toList)
  }.toList

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
      avgNumDisagreed = hi.hit.prompt.qaPairs.size - nonWorkerAssignments.map(numAgreed(workerAssignment, _)).mean
    } yield (HITInfo(hi.hit, workerAssignment :: nonWorkerAssignments), avgNumDisagreed)
    scored.sortBy(_._2).map(_._1)
  }

  def currentGenSentences: List[(SentenceId, String)] = {
    genHelper.activeHITInfosByPromptIterator.map(_._1.id).map(id =>
      id -> TextRendering.renderSentence(getTokensForId(id))
    ).toList
  }

}

object FinalExperiment {
  val valQualTestString = s"""<?xml version="1.0" encoding="UTF-8"?>
<QuestionForm xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionForm.xsd">
<Overview>
<Title>Answering simple questions about a sentence</Title>
<Text>Please carefully read over the instructions for our task named "Answer simple questions about a sentence". This qualification test will evaluate your understanding of those instructions, focusing on some of the trickier cases. It is very important for us that you follow the guidelines because you will be helping us detect question writers who do not correctly understand the task. The guidelines are also important because everyone has slightly different intuitions about how to answer these questions, but we need everyone to perform consistently so that you can accurately be judged by how well you agree with each other.
</Text>
<Text>It's a good idea have a tab open with the task preview so you can consult the instructions during this test. Feel free to take it as many times as necessary; your score can be a good form of feedback on how well you understand the expectations.</Text>
<Text>Suppose you get a HIT with the following sentence and list of questions. Please provide a judgment for each:</Text>
<Text>"According to the Nonexistent Centre for Imperialism Studies, exploitation colonialism involves fewer colonists and focuses on access to resources for export, typically to the metropole." </Text>
</Overview>

<Question>
  <QuestionIdentifier>q1</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>1. Exploitation colonialism focuses on?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q1-a1</SelectionIdentifier>
        <Text>access to resources for export, typically to the metropole</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q1-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q2</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>2. What involves fewer colonists?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q2-a1</SelectionIdentifier>
        <Text>Exploitation colonialism</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q2-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q3</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>3. How many colonists?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q3-a1</SelectionIdentifier>
        <Text>fewer</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q3-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q4</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>4. What kind of colonialism?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q4-a1</SelectionIdentifier>
        <Text>Exploitation</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q4-redundant</SelectionIdentifier>
        <Text>N/A: Redundant with question 2</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q4-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q5</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>5. What form of colonialism involves fewer colonists?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q5-a1</SelectionIdentifier>
        <Text>Exploitation</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q5-redundant1</SelectionIdentifier>
        <Text>N/A: Redundant with question 2</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q5-redundant2</SelectionIdentifier>
        <Text>N/A: Redundant with question 4</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q5-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q6</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>6. Fewer what?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q6-a1</SelectionIdentifier>
        <Text>colonists</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q6-redundant</SelectionIdentifier>
        <Text>N/A: Redundant with question 3</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q6-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q7</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>7. What is exploited?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q7-a1</SelectionIdentifier>
        <Text>colonialism</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q7-a2</SelectionIdentifier>
        <Text>resources</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q7-redundant1</SelectionIdentifier>
        <Text>N/A: Redundant with question 4</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q7-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q8</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>8. Where do the exports typically go?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q8-a1</SelectionIdentifier>
        <Text>the metropole</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q8-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q9</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>9. Does it focus more on colonists or access to resources?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q9-a1</SelectionIdentifier>
        <Text>colonists</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q9-a2</SelectionIdentifier>
        <Text>access to resources</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q9-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q10</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>10. What gets exported?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q10-a1</SelectionIdentifier>
        <Text>access to resources</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q10-a2</SelectionIdentifier>
        <Text>resources</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q10-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q11</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>11. What is the last word of the Centre?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q11-a1</SelectionIdentifier>
        <Text>Studies</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q11-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q12</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>12. What is the Centre's full name?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q12-a1</SelectionIdentifier>
        <Text>Nonexistent Centre for Imperialism Studies</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q12-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q13</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>13. What does the Centre study?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q13-a1</SelectionIdentifier>
        <Text>Imperialism</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q13-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

<Question>
  <QuestionIdentifier>q14</QuestionIdentifier>
  <IsRequired>true</IsRequired>
  <QuestionContent><Text>14. Is the Centre existent?</Text></QuestionContent>
  <AnswerSpecification>
    <SelectionAnswer>
      <StyleSuggestion>radiobutton</StyleSuggestion>
      <Selections>

        <Selection>
        <SelectionIdentifier>q14-a1</SelectionIdentifier>
        <Text>Nonexistent</Text>
        </Selection>

        <Selection>
        <SelectionIdentifier>q14-invalid</SelectionIdentifier>
        <Text>N/A: Invalid question</Text>
        </Selection>

      </Selections>
    </SelectionAnswer>
  </AnswerSpecification>
</Question>

</QuestionForm>
""".trim
  private[this] def answerXML(qid: String, aid: String): String = answerXML(qid, List(aid))
  private[this] def answerXML(qid: String, aids: List[String]): String = {
    val opts = aids.map(aid => s"""
<AnswerOption>
  <SelectionIdentifier>$aid</SelectionIdentifier>
  <AnswerScore>1</AnswerScore>
</AnswerOption>
""".trim).mkString("\n")
    s"""
<Question>
<QuestionIdentifier>$qid</QuestionIdentifier>
$opts
</Question>
""".trim

  }
  val valQualAnswerKeyString = s"""<?xml version="1.0" encoding="UTF-8"?>
<AnswerKey xmlns="http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd">
${answerXML("q1", "q1-invalid")}
${answerXML("q2", "q2-a1")}
${answerXML("q3", "q3-a1")}
${answerXML("q4", "q4-a1")}
${answerXML("q5", "q5-redundant2")}
${answerXML("q6", "q6-a1")}
${answerXML("q7", List("q7-invalid", "q7-a2"))}
${answerXML("q8", "q8-a1")}
${answerXML("q9", "q9-invalid")}
${answerXML("q10", "q10-a2")}
${answerXML("q11", "q11-invalid")}
${answerXML("q12", "q12-a1")}
${answerXML("q13", "q13-a1")}
${answerXML("q14", "q14-invalid")}
<QualificationValueMapping>
  <PercentageMapping>
    <MaximumSummedScore>14</MaximumSummedScore>
  </PercentageMapping>
</QualificationValueMapping>
</AnswerKey>
""".trim

}
