package mts.experiments.expH

import mts.analysis._
import mts.experiments._
import mts.core._
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

  val genAccQualTypeName = "Question-answer writing accuracy % (auto-granted)"
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

  val valAgrQualTypeName = "Question answering agreement % (auto-granted)"
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

  val valTestQualTypeName = if(config.isProduction) "Question answering test score (%)"
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
      QualTest.valQualTestString, // test: QuestionForm
      QualTest.valQualAnswerKeyString, // AnswerKey
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

  lazy val ptb100ForAMR = FileManager.allPTBSentencePaths.take(103).map(PTBSentenceId.apply).toList

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
      Write more question-answer pairs for increasing bonuses, and
      maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
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
      Maintain high agreement with others to stay qualified.
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

  lazy val allPrompts = sourceIds
    .flatMap(id => idSplits(id).map(GenerationPrompt(id, _)))

  lazy val sourcePrompts = (
    ptb100ForAMR ++ (
      sourceIds
        .drop(860) // those done in the version with blocks
        .drop(414) // those done before I had the auto-grantedness of the qual explicitly written
    )
  ).flatMap(id => idSplits(id).map(GenerationPrompt(id, _)))

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

  // for use while it's running

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

  // convenience functions

  val oldGenHITTypeId = "3X8M0CO8US8JERH7QA0GGQIWAEHPVL"
  val oldValHITTypeId = "3OR5EJIUG2QY9PC04VUEYEGYR3Q9UL"

  val oldGenHITTypeId2 = "36SUH4ZPJUVEFKCRRRIAVB1LGOZ705"
  val oldValHITTypeId2 = "3XRW87W7OXAP1BEXLSFQFDKLIPNTQ0"

  lazy val oldGenInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] =
    (FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](oldGenHITTypeId)
       ++ FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](oldGenHITTypeId2))
  lazy val oldValInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] =
    (FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](oldValHITTypeId)
       ++ FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](oldValHITTypeId2))

  def allGenInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] =
    oldGenInfos ++ FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId)
  def allValInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] = oldValInfos ++ FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId)

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
      questionTokens = tokenize(question).toVector
      questionSentenceAlignments = getQuestionSentenceAlignments(sentence, questionTokens) // q-s
      qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
      _ <- append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${genWorkerId}\t")
      _ <- append(s"${sentence(keywordIndex)} ($keywordIndex)\t$qsAlignmentsString\t${questionTokens.mkString(" ")}\t")
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
    val allIds = genInfos.map(_.hit.prompt.id).collect {
      case id @ WikiSentenceId(_) => id
    }.toSet.toList
    val trainIds = allIds.filter(isTrain)
    val devIds = allIds.filter(isDev)
    val testIds = allIds.filter(isTest)
    FileManager.saveDataFile(experimentName, "train.tsv", makeTSV(trainIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "dev.tsv", makeTSV(devIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "test.tsv", makeTSV(testIds, genInfos, valInfos))
  }

  def writePTBTSVs = {
    val genInfos = allGenInfos
    val valInfos = allValInfos
    val trainIds = ptbTrain.map(PTBSentenceId.apply).toList
    val devIds = ptbDev.map(PTBSentenceId.apply).toList
    val testIds = ptbTest.map(PTBSentenceId.apply).toList
    FileManager.saveDataFile(experimentName, "ptb-train.tsv", makeTSV(trainIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "ptb-dev.tsv", makeTSV(devIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "ptb-test.tsv", makeTSV(testIds, genInfos, valInfos))
  }


  // assumes nonempty span ... checks it though
  def getOffsetAndSpan(reference: Seq[String], span: Set[Int]) = {
    import scalaz._
    import Scalaz._
    if(span.isEmpty) {
      System.err.println("Identifying offset of empty span for reference:\n" + TextRendering.renderSentence(reference))
    }
    if(span.exists(i => i < 0 || i >= reference.size)) {
      System.err.println("Identifying offset of span containing indices outside of reference:\n" +
                           TextRendering.renderSentence(reference) + "\n" +
                           span.mkString(" "))
    }

    @Lenses case class OffsetState(curOffset: Int, inSpan: Boolean, beginOffset: Int, phrase: String)
    type ST[A] = State[OffsetState, A]
    val firstWord = span.min
    val lastWord = span.max
    def possiblyAddToPhrase(text: String) =
      State.modify(OffsetState.curOffset.modify(_ + text.length)) >>
        State.modify(s =>
          if(s.inSpan) OffsetState.phrase.modify(_ + text)(s) else s
        )
    def emitToken(token: String, index: Int): ST[String] = {
      val normalizedToken = TextRendering.normalizeToken(token)
      for {
        _ <- State.modify(if(index == firstWord) OffsetState.inSpan.set(true) else identity[OffsetState])
        _ <- State.modify(if(index == firstWord) (s: OffsetState) => OffsetState.beginOffset.set(s.curOffset)(s)
                          else identity[OffsetState])
        _ <- possiblyAddToPhrase(normalizedToken)
        _ <- State.modify(if(index == lastWord) OffsetState.inSpan.set(false) else identity[OffsetState])
      } yield normalizedToken
    }

    val OffsetState(_, _, begin, phrase) = TextRendering.renderSentenceM[(String, Int), ST, String](
      reference.zipWithIndex,
      _._1,
      _ => emitToken(" ", -1),
      Function.tupled(emitToken)
    ).exec(OffsetState(0, false, -1, ""))

    val sentence = TextRendering.renderSentence(reference)
    val reproPhrase = sentence.substring(begin, math.min(begin + phrase.length, sentence.length))
    if(reproPhrase != phrase) {
        System.err.println(
          s"Problem for sentence\n$sentence \nGiven answer:\n$phrase \nRepro answer:\n$reproPhrase")
    }

    (begin, phrase)
  }

  def squadFormattedFile(validQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]]): String = {
    import argonaut._
    import Argonaut._
    val idsByFile = validQAs.keys.collect {
      case id @ WikiSentenceId(wikiPath) => id
    }.groupBy(_.path.filePath)

    def getAnswerSpanJson(tokens: Vector[String], answer: Set[Int]) = {
      val filledOutAnswer = (answer.min to answer.max).toSet
      val renderedSentence = TextRendering.renderSentence(tokens)
      val (answerStart, answerText) = getOffsetAndSpan(tokens, filledOutAnswer)
      // stuff looked good (better, in fact, bc of treatment of quotes). if there are more problems, uncomment this and investigate.
      // val otherText = TextRendering.renderSpan(tokens, filledOutAnswer).trim
      // if(!answerText.equals(otherText)) {
      //   System.err.println(
      //     s"Problem for sentence\n${TextRendering.renderSentence(tokens)}\nExpected answer:\n$otherText \nPrinted answer:\n$answerText")
      // }
      Json.obj(
        "answer_start" -> jNumber(answerStart),
        "text" -> jString(answerText)
      )
    }

    def getQAJson(sentenceId: WikiSentenceId, sentenceTokens: Vector[String], qIndex: Int, wqa: WordedQAPair, answers: List[Set[Int]]) = {
      Json.obj(
        "answers" -> Json.array(answers.map(a => getAnswerSpanJson(sentenceTokens, a)): _*),
        "question" -> jString(wqa.question),
        "id" -> jString(s"${sentenceId.readableFileString}::${sentenceId.readableSentenceIndex}::$qIndex")
      )
    }

    def getSentenceJson(sentenceId: WikiSentenceId) = {
      val sentenceTokens = getTokensForId(sentenceId)
      val qas = validQAs(sentenceId).zipWithIndex.map {
        case ((wqa, answers), qIndex) => getQAJson(sentenceId, sentenceTokens, qIndex, wqa, answers)
      }.toSeq

      Json.obj(
        "context" -> jString(TextRendering.renderSentence(getTokensForId(sentenceId))),
        "qas" -> Json.array(qas: _*)
      )
    }

    val files: Seq[Json] = idsByFile.keys.toSeq.map { filePath =>
      val wikiFile = FileManager.getWiki1kFile(filePath).get
      val title = wikiFile.title
      val sentenceIds = idsByFile(filePath)
      val sentenceJsons = sentenceIds.map(getSentenceJson)
      Json.obj(
        "title" -> jString(title),
        "paragraphs" -> Json.array(sentenceJsons.toSeq: _*)
      )
    }

    val result = Json.obj(
      "data" -> Json.array(files: _*),
      "version" -> jString("1.1")
    )

    result.nospaces
  }

  def writeAllSquadFormatted = {
    val allIds = allGenInfos.map(_.hit.prompt.id).collect {
      case id @ WikiSentenceId(_) => id
    }.toSet.toList
    val trainQAs = validQAs.filter(p => isTrain(p._1))
    val devQAs = validQAs.filter(p => isDev(p._1))
    val testQAs = validQAs.filter(p => isTest(p._1))
    FileManager.saveDataFile(experimentName, "squad-train.json", squadFormattedFile(trainQAs))
    FileManager.saveDataFile(experimentName, "squad-dev.json",  squadFormattedFile(devQAs))
    FileManager.saveDataFile(experimentName, "squad-test.json",  squadFormattedFile(testQAs))
  }

  /* do not use any of these above or outside the console since they become outdated */

  lazy val alignedInfos: Map[
    SentenceId,
    Map[GenerationPrompt,
        Map[Assignment[List[WordedQAPair]],
            List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]]]
  ] = {
    val genInfos = allGenInfos
    val valInfos = allValInfos

    sourceIds.map { id =>
      id -> allPrompts.filter(_.id == id).map { prompt =>
        prompt -> genInfos.filter(_.hit.prompt == prompt).flatMap(_.assignments).map { genAssignment =>
          genAssignment -> valInfos.filter(_.hit.prompt.sourceAssignmentId == genAssignment.assignmentId)
        }.toMap
      }.toMap
    }.toMap
  }

  lazy val trainingAlignedInfos = alignedInfos.filter {
    case (id @ WikiSentenceId(_), _) => isTrain(id)
    case _ => false
  }.toMap

  lazy val alignedQAs: Map[SentenceId, Map[WordedQAPair, List[ValidationAnswer]]] = {
    val genInfosByPrompt = allGenInfos.groupBy(_.hit.prompt)
    val valInfosByGenAssignment = allValInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    allPrompts.groupBy(_.id).flatMap { case (id, prompts) =>
      val qaToAnswers = for {
        prompt <- prompts
        genInfo <- genInfosByPrompt.get(prompt).toList.flatten
        assignment <- genInfo.assignments
        validationResponses = for {
          valInfo <- valInfosByGenAssignment.get(assignment.assignmentId).toList.flatten
          assignment <- valInfo.assignments
        } yield assignment.response
        pair <- assignment.response.zip(validationResponses.transpose)
      } yield pair
      qaToAnswers.onlyIf(_.nonEmpty).map(id -> _.toMap)
    }.toMap
  }

  lazy val trainingAlignedQAs = alignedQAs.filter {
    case (id @ WikiSentenceId(_), _) => isTrain(id)
    case _ => false
  }.toMap

  lazy val validQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]] = {
    alignedQAs.map { case (id, qasToAnswers) =>
      id -> qasToAnswers.flatMap { case (wqa, vAnswers) =>
        vAnswers
          .onlyIf(_.forall(_.isAnswer))
          .map(_.flatMap(_.getAnswer).map(_.indices))
          .map(wqa -> _)
      }.toMap
    }.toMap
  }

  lazy val trainValidQAs = validQAs.filter {
    case (id @ WikiSentenceId(_), _) => isTrain(id)
    case _ => false
  }.toMap

  lazy val devValidQAs = validQAs.filter {
    case (id @ WikiSentenceId(_), _) => isDev(id)
    case _ => false
  }.toMap

  def renderSentenceEntry(id: SentenceId, qas: Map[WordedQAPair, List[Set[Int]]]) = {
    qas.map { case (wqa, as) => renderValidQA(getTokensForId(id), wqa, as) }.mkString("\n")
  }

  def renderValidQA(tokens: Vector[String], wqa: WordedQAPair, validations: List[Set[Int]]) = {
    val sentenceString = TextRendering.renderSentence(tokens)
    val answerString = TextRendering.renderSpan(tokens, wqa.answer)
    val validationStrings = validations.map(TextRendering.renderSpan(tokens, _)).mkString(" \t")
    s"$sentenceString \n\t${wqa.question} --> $answerString \t|$validationStrings"
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
        answer <- wqa.answer :: qResponses.flatMap(_.getAnswer).map(_.indices)
      } yield WordedQAPair(wqa.wordIndex, wqa.question, answer)
    }
    qas
  }

  // def allQAPairSamplings(id: SentenceId, n: Int = 1) = {
  //   val promptToAlignments = alignedInfos(id)
  //   val sentence = getTokensForId(id)
  //   val qas = promptToAlignments.values.flatMap { alignment =>
  //     // sample n assignments
  //     val samples = alignment.keys.toVector.combinations(n)
  //     samples.map { sample =>
  //       for {
  //         genAssignment <- sample
  //         valResponses = alignment(genAssignment).flatMap(_.assignments).map(_.response)
  //                                                                           (wqa, index) <- genAssignment.response.zipWithIndex
  //         qResponses = valResponses.map(_(index))
  //         if qResponses.forall(!_.isInvalid)
  //         answer <- wqa.answer :: qResponses.flatMap(_.getAnswer).map(_.indices)
  //       } yield WordedQAPair(wqa.wordIndex, wqa.question, answer)
  //     }
  //   }
  //   qas
  // }

  class QuestionModeling(theseValidQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]]) {
    def validQAsForNumQuestionWords(p: Int => Boolean) = theseValidQAs.map { case (id, qas) =>
      val tokens = getTokensForId(id)
      id -> qas.filter { case (wqa, answers) => p(getWordsInQuestion(tokens, wqa.question).size) }
    }.toMap

    def getExternalVocabulary(id: SentenceId, qas: Map[WordedQAPair, List[Set[Int]]]) = {
      val tokens = getTokensForId(id)
      qas.flatMap { case (wqa, as) =>
        val qTokens = tokenize(wqa.question.toLowerCase).toVector
        qTokens.indices
          .filterNot(getAlignedQuestionIndices(tokens, qTokens))
          .map(qTokens.apply _)
      }.toList
    }

    def externalWordReport(sum: Double)(word: String, count: Int) =
      f"$word%s\t$count%d\t${count.toDouble / sum}%.4f"

    lazy val externalWordCounts = theseValidQAs.iterator.flatMap(
      Function.tupled(getExternalVocabulary)
    ) <| Scorer.apply[String, Int]

    lazy val externalWordReports = externalWordCounts.iterator.toVector
      .sortBy(-_._2)
      .map(Function.tupled(externalWordReport(externalWordCounts.sum)))

    lazy val externalNonStopwordCounts = theseValidQAs.iterator.flatMap(
      Function.tupled(getExternalVocabulary)
    ).filterNot(isReallyUninteresting) <| Scorer.apply[String, Int]

    lazy val externalNonStopwordReports = externalNonStopwordCounts.iterator.toVector
      .sortBy(-_._2)
      .map(Function.tupled(externalWordReport(externalNonStopwordCounts.sum)))

    lazy val allValidQuestions = theseValidQAs.map {
      case (id, qaMap) => id -> qaMap.keys.map(wqa => posTag(tokenize(wqa.question)))
    }.toMap

    lazy val numValidQuestions = allValidQuestions.map(_._2.size).sum

    class NGramReport(tokenizedStrings: Iterator[List[String]]) {
      lazy val prefixes = tokenizedStrings.flatMap(tokens =>
        (tokens ++ List("<End>")).inits.filter(_.nonEmpty)
      ) <| Scorer.apply[List[String], Int]

      lazy val orderedPrefixes = prefixes.iterator.toVector.sortBy(-_._2)

      def prefixReport(prefix: List[String], count: Int) =
        f"${prefix.mkString(" ")}%s\t$count%d\t${count.toDouble / numValidQuestions}%.4f"

      lazy val orderedPrefixReports = orderedPrefixes.map(Function.tupled(prefixReport))
    }

    lazy val questionNGrams = new NGramReport(allValidQuestions.iterator.flatMap(_._2).map(_.map(_.token.toLowerCase)))

    lazy val collapsedQuestions = allValidQuestions.map {
      case (id, questions) => id -> questions.map { q =>
        val alignedTokens = getAlignedQuestionIndices(getTokensForId(id), q.map(_.token).toVector)
        val collapsedTokens = q.zipWithIndex.foldRight(List.empty[POSTaggedToken]) { case ((posToken, index), acc) =>
          if(alignedTokens.contains(index)) {
            if(acc.headOption.fold(true)(_.token != "<>")) POSTaggedToken("<>", "<>") :: acc
            else acc
          } else posToken.copy(token = posToken.token.toLowerCase) :: acc
        }
        collapsedTokens
      }
    }

    lazy val collapsedQuestionNGrams = new NGramReport(collapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

    lazy val auxCollapsedQuestions = collapsedQuestions.map {
      case (id, cQuestions) => id -> cQuestions.map { tokens =>
        tokens.map { case t =>
          if(inflections.isCopulaVerb(t.token.lowerCase)) POSTaggedToken("<be>", "<be>")
          else if(Inflections.doVerbs.contains(t.token.lowerCase)) POSTaggedToken("<do>", "<do>")
          else t
        }
      }
    }

    lazy val auxCollapsedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

    lazy val delexicalizedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.pos)))

    // val determiners = Set("the", "a", "this")
    // val pronouns = Set("i", "we", "you", "he", "she", "him", "her", "it", "something", "someone", "they", "them")
    // val kindCats = Set("kind", "type", "types")
    // val whCats = Set("year", "country", "part", "month", "day", "people", "nationality", "city", "place", "thing",
    //                  "group", "event", "time", "number", "man", "things", "language", "person", "album", "position",
    //                  "animal", "years", "state", "size", "color", "score", "percentage", "date", "gender", "countries",
    //                  "direction", "organization", "level", "religion", "profession", "company", "job")
    // val ofCats = Set("name", "title")
    // val howCats = Set("long", "old")

    def delexicalizeQuestion(id: SentenceId, question: String) = {
      val qTokens = tokenize(question)
      val sentenceTokens = getTokensForId(id)
      val alignedQIs = getAlignedQuestionIndices(sentenceTokens, qTokens.toVector)
      val posTagged = posTag(qTokens)
      posTagged.zipWithIndex.map {
        case (POSTaggedToken(token, pos), index) =>
          if(alignedQIs.contains(index)) (token, s"*$pos")
          else if(inflections.isCopulaVerb(token.lowerCase)) (token, "<be>")
          else if(Inflections.doVerbs.contains(token.lowerCase)) (token, "<do>")
          else (token, pos)
      }
    }

    def writeTemplatedQuestions(filename: String) = {
      val sb = new StringBuilder
      for {
        (id, qaPairToAnswers) <- theseValidQAs
        sentenceTokens = posTag(getTokensForId(id).toList)
        (wqa, answers) <- qaPairToAnswers
        (qTokens, qTags) = delexicalizeQuestion(id, wqa.question).unzip
        alignments = getQuestionSentenceAlignments(sentenceTokens.toVector.map(_.token), qTokens.toVector)
        sQuestionIndices = alignments.map(_._2)
        qSentenceIndices = alignments.map(_._1)
        answer <- wqa.answer :: answers
      } yield {
        val sTags = sentenceTokens.zipWithIndex.map { case(POSTaggedToken(_, pos), i) =>
          val placementTag = if(sQuestionIndices(i)) "Q" else if(answer(i)) "A" else "O"
          s"$placementTag-$pos"
        }.mkString(" ")
        val line = s"${sentenceTokens.map(_.token).mkString(" ")} ||| $sTags ||| ${qTokens.mkString(" ")} ||| ${qTags.mkString(" ")}\n"
        sb.append(line)
      }

      FileManager.saveDataFile(experimentName, filename, sb.toString)
    }

  }

  lazy val trainQuestionModeling = new QuestionModeling(trainValidQAs)
  lazy val devQuestionModeling = new QuestionModeling(devValidQAs)

  object PTBAnalysis {

    lazy val finishedPTBIds = alignedInfos.collect {
      case (id @ PTBSentenceId(path), infosByPrompt)
          if infosByPrompt.forall(_._2.size == 5) => id
    }.toSet

    lazy val validPTBQAs = validQAs.filter {
      case (id @ PTBSentenceId(_), _) => finishedPTBIds.contains(id)
      case _ => false
    }

    def alignToPAS(
      words: Vector[String],
      qas: List[WordedQAPair],
      paStructures: List[PredicateArgumentStructure]) = {
      case class PredArg(pred: Predicate, arg: ArgumentSpan)
      val predArgs = paStructures
        .flatMap(pas => pas.arguments.map(PredArg(pas.pred, _)))
        .filterNot(pa => labelIsIrrelevant(pa.arg.label))
        .filterNot(pa => Inflections.auxiliaryVerbs.contains(pa.pred.head.token.lowerCase))
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
      val numPredictions = alignedPAs.filter(_ == None).size// + pasCovered.size // TODO why was I adding this?

      val sentenceString = TextRendering.renderSentence(words)
      val missedDeps = predArgs.filterNot(pasCovered).mkString("\n")
      println(s"\n\n$sentenceString\nMissed deps:\n$missedDeps")

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

    def allPropBankPRs(n: Int = 1) = finishedPTBIds.map { case id @ PTBSentenceId(path) =>
      val sentence = getTokensForId(id)
      val qas = sampleQAPairs(id, n)
      propBankPR(path, qas.toList)
    }.toList

    lazy val pbRecalls = (1 to 5).map(i => List.fill((2 * (5 - i)) + 1)(allPropBankPRs(i).reduce(_ aggregate _).recall))
    lazy val pbRecallDists = pbRecalls.map(r => (r.mean, r.stdev))
    lazy val pbRecallReport = pbRecallDists.zip(1 to 5)
      .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
      .mkString("\n")

    def nomBankPR(path: PTBSentencePath, qas: List[WordedQAPair]): PrecisionRecall = {
      import mts.datasets.nombank._
      FileManager.getNomBankPredArgStructuresReindexed(path).toOption
        .fold(PrecisionRecall(qas.size, 0, 0, 0)) { paStructures =>
        alignToPAS(getTokensForId(PTBSentenceId(path)), qas, paStructures)
      }
    }

    def allNomBankPRs(n: Int = 1) = finishedPTBIds.map { case id @ PTBSentenceId(path) =>
        val sentence = getTokensForId(id)
        val qas = sampleQAPairs(id, n)
        nomBankPR(path, qas.toList)
    }.toList

    lazy val nbRecalls = (1 to 5).map(i => List.fill((2 * (5 - i)) + 1)(allNomBankPRs(i).reduce(_ aggregate _).recall))
    lazy val nbRecallDists = nbRecalls.map(r => (r.mean, r.stdev))
    lazy val nbRecallReport = nbRecallDists.zip(1 to 5)
      .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
      .mkString("\n")

    // only 5 sentences have coref data :( should do that qualitatively

    // qasrl comparison too

    lazy val fullReport: String = {
      s"PropBank:\n$pbRecallReport \nNombank:\n$nbRecallReport"
    }
  }

  // if do manual annotation interface...
  // - propbank
  // - nombank
  // - coref
  // - relations
  // - coordination scope
  // - pp attachment? probably not interesting enough...

  object ValidationStats {
    lazy val assignmentCosts = allValInfos.map {
      case HITInfo(hit, assignments) =>
        val numSpecialWords = hit.prompt.genPrompt.keywords.size
        val numQAsProvided = hit.prompt.qaPairs.size
        val numQAsValid = math.round(assignments.map(_.response.filter(_.isAnswer).size).mean - 0.01).toInt
        val genBonus = (1 to (numQAsValid - numSpecialWords)).map(bonusFor).sum
        val valReward = 0.15 + (0.03 * math.max(0, numQAsProvided - 4))
        0.20 + genBonus + (2 * valReward)
    }

    lazy val assignmentNumQAs = allValInfos.map {
      case HITInfo(hit, assignments) => hit.prompt.qaPairs.size
    }

    lazy val assignmentNumValids = allValInfos.map {
      case HITInfo(hit, assignments) =>
        math.round(assignments.map(_.response.filter(_.isAnswer).size).mean - 0.01).toInt
    }

    lazy val assignmentNumRedundants = allValInfos.map {
      case HITInfo(hit, assignments) =>
        math.round(assignments.map(_.response.filter(_.isRedundant).size).mean).toInt
    }

    lazy val assignmentNumBothAnswered = allValInfos.map(
      _.assignments
        .map(a => a.response.map(resolveRedundancy(_, a.response)))
        .transpose
        .filter(_.forall(_.isAnswer))
        .size)

    lazy val assignmentNumBothAnsweredAndNoIntersection = allValInfos.map(
      _.assignments
        .map(a => a.response.map(resolveRedundancy(_, a.response)))
        .transpose
        .filter(_.forall(_.isAnswer))
        .filter(_.flatMap(_.getAnswer).map(_.indices).reduce(_ intersect _).isEmpty)
        .size)

    lazy val assignmentNumAgreements = allValInfos.map(hi => numAgreed(hi.assignments(0), hi.assignments(1)))

    lazy val assignmentNumQAPairsEachSpecialWordInHIT = for {
      HITInfo(hit, assignments) <- allGenInfos
      sentence = getTokensForId(hit.prompt.id)
      assignment <- assignments
      keyword <- hit.prompt.keywords
    } yield assignment.response
      .map(wqa => getWordsInQuestion(sentence, wqa.question) ++ wqa.answer)
      .filter(_.contains(keyword)).size

    lazy val totalNumKeywordAssignments = allGenInfos
      .map(hi => hi.assignments.size + hi.hit.prompt.keywords.size)
      .sum

    lazy val assignmentsMissingSpecialWords = for {
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

    def pct(num: Int, denom: Int): String =
      f"$num%d (${num * 100.0 / denom}%.2f)"
    def dist[N](iter: Seq[N])(implicit N : Numeric[N]): String =
      f"${N.toDouble(iter.sum)}%.2f (${iter.mean}%.2f ± ${iter.stdev}%.4f)"

    lazy val report = f"""
Total cost: ${dist(assignmentCosts)}%s
Number of questions: ${dist(assignmentNumQAs)}%s
Number of valid questions: ${pct(assignmentNumBothAnswered.sum, assignmentNumQAs.sum)}%s
Number of validator agreements: ${pct(assignmentNumAgreements.sum, assignmentNumQAs.sum)}%s
Number of valid questions with no validator overlap: ${pct(assignmentNumBothAnsweredAndNoIntersection.sum, assignmentNumBothAnswered.sum)}%s
Number of keywords: $totalNumKeywordAssignments%d
Number of keywords missing: ${pct(assignmentsMissingSpecialWords.size, totalNumKeywordAssignments)}%s
""".trim
  }

  // scatter : length / num QA pairs

  object CoverageStats {

    def orderedPairsCovered(sentence: Vector[String], question: String, answerIndices: Set[Int]) = {
      val pairs = for {
        qi <- getWordsInQuestion(sentence, question)
        ai <- answerIndices
      } yield (math.min(qi, ai), math.max(qi, ai))
      pairs.toSet
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

    lazy val coverageCountsBySentence = {
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

    lazy val avgQAsPerKeywordByWorker = allGenInfos.flatMap(_.assignments).groupBy(_.workerId).map {
      case (worker, as) => worker -> {
        val nums = as.flatMap(_.response).groupBy(_.wordIndex).map(_._2.size).toList
        (nums.mean, nums.size)
      }
    }

  }

  // lazy val assignmentNumOneQuestionWord = allValInfos.map {
  //   case HITInfo(hit, assignments) => hit.prompt.qaPairs.size
  // }
}
