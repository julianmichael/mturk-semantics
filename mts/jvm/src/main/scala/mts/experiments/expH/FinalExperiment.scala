package mts.experiments.expH

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
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
  // val numPTB = 5

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
  // val numWikipedia = 5

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
  // val numWikinews = 5

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
    TaskIndex.expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt,
    frozenHITTypeId = Some("3554GQY3BJXDVEL54N24OMP560NSLM"))

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
    TaskIndex.expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt,
    frozenHITTypeId = Some("3AYVNGH59IZRTO9MQCW5NV51ECHYDQ"))

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

  // actual
  lazy val allIds = ptb100ForAMR ++ sourceIds

  lazy val allPrompts = allIds
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
      id <- allIds.iterator
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

  lazy val allGenInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]] =
    oldGenInfos ++ FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId)
  lazy val allValInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]] = oldValInfos ++ FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId)

  def allSentenceStats: Map[SentenceId, SentenceStats] = {
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

  def makeTSV(
    ids: List[SentenceId],
    genInfos: List[HITInfo[GenerationPrompt, List[WordedQAPair]]],
    valInfos: List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]
  ): String = {
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
      (genWorkerId, keywords, WordedQAPair(keywordIndex, question, answerIndices), valAnswers, valAnswersString, valFeedback) <- iter {
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
          genHIT.prompt.keywords,
          wqa,
          valAnswers,
          valAnswers.map(valAnswer =>
            renderValidationAnswer(sentence, valAnswer, genAssignment.response)
          ).mkString("\t"),
          valFeedback.mkString("\t")
        )
        qaPairs.sortBy(_._3.wordIndex)
      }
      questionTokens = tokenize(question).toVector
      questionSentenceAlignments = getQuestionSentenceAlignments(sentence, questionTokens) // q-s
      qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
      _ <- append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${keywords.toVector.sorted.mkString(" ")}\t${genWorkerId}\t")
      _ <- append(s"${sentence(keywordIndex)} ($keywordIndex)\t$qsAlignmentsString\t${questionTokens.mkString(" ")}\t${valAnswers.size + 1}\t")
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
    val amrIds = ptb100ForAMR.toList
    FileManager.saveDataFile(experimentName, "ptb-train.tsv", makeTSV(trainIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "ptb-dev.tsv", makeTSV(devIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "ptb-test.tsv", makeTSV(testIds, genInfos, valInfos))
    FileManager.saveDataFile(experimentName, "ptb-amr.tsv", makeTSV(amrIds, genInfos, valInfos))
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

  /* do not use any of these above or outside the console since they become outdated */

  lazy val alignedInfos: Map[
    SentenceId,
    Map[GenerationPrompt,
        Map[Assignment[List[WordedQAPair]],
            List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]]]
  ] = {
    val genInfos = allGenInfos
    val valInfos = allValInfos

    allIds.map { id =>
      id -> allPrompts.filter(_.id == id).map { prompt =>
        prompt -> genInfos.filter(_.hit.prompt == prompt).flatMap(_.assignments).map { genAssignment =>
          genAssignment -> valInfos.filter(_.hit.prompt.sourceAssignmentId == genAssignment.assignmentId)
        }.toMap
      }.toMap
    }.toMap
  }

  // lazy val trainingAlignedInfos = alignedInfos.filter {
  //   case (id @ WikiSentenceId(_), _) => isTrain(id)
  //   case _ => false
  // }.toMap

  lazy val alignedQAs: Map[SentenceId, Map[WordedQAPair, List[ValidationAnswer]]] = {
    val genInfosByPrompt = allGenInfos.groupBy(_.hit.prompt)
    val valInfosByGenAssignment = allValInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    allPrompts.groupBy(_.id).filter(p => allIds.contains(p._1)).flatMap { case (id, prompts) =>
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

  // lazy val trainAlignedQAs = alignedQAs.filter {
  //   case (id @ WikiSentenceId(_), _) => isTrain(id)
  //   case _ => false
  // }.toMap

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

  // lazy val trainValidQAs = validQAs.filter {
  //   case (id @ WikiSentenceId(_), _) => isTrain(id)
  //   case _ => false
  // }.toMap

  // lazy val devValidQAs = validQAs.filter {
  //   case (id @ WikiSentenceId(_), _) => isDev(id)
  //   case _ => false
  // }.toMap

  // lazy val testValidQAs = validQAs.filter {
  //   case (id @ WikiSentenceId(_), _) => isTest(id)
  //   case _ => false
  // }.toMap

  def renderQAs(id: SentenceId, qas: Map[WordedQAPair, List[Set[Int]]]) = {
    val sentence = getTokensForId(id)
    TextRendering.renderSentence(sentence) + "\n" +
      qas.map { case (WordedQAPair(kwIndex, question, answerIndices), valAnswers) =>
        val answerStrings = (answerIndices :: valAnswers).map(TextRendering.renderSpan(sentence, _)).mkString(" \t| ")
        s"\t$question --> \t$answerStrings"
      }.mkString("\n")
  }

  // TODO think these are repetitive and can be removed
  // def renderSentenceEntry(id: SentenceId, qas: Map[WordedQAPair, List[Set[Int]]]) = {
  //   qas.map { case (wqa, as) => renderValidQA(getTokensForId(id), wqa, as) }.mkString("\n")
  // }

  // def renderValidQA(tokens: Vector[String], wqa: WordedQAPair, validations: List[Set[Int]]) = {
  //   val sentenceString = TextRendering.renderSentence(tokens)
  //   val answerString = TextRendering.renderSpan(tokens, wqa.answer)
  //   val validationStrings = validations.map(TextRendering.renderSpan(tokens, _)).mkString(" \t")
  //   s"$sentenceString \n\t${wqa.question} --> $answerString \t|$validationStrings"
  // }

  // def sampleQAPairs(id: SentenceId, n: Int = 1) = {
  //   val promptToAlignments = alignedInfos(id)
  //   val sentence = getTokensForId(id)
  //   val qas = promptToAlignments.values.flatMap { alignment =>
  //     // sample n assignments
  //     val sample = util.Random.shuffle(alignment.keys.toVector).take(n)
  //     for {
  //       genAssignment <- sample
  //       valResponses = alignment(genAssignment).flatMap(_.assignments).map(_.response)
  //       (wqa, index) <- genAssignment.response.zipWithIndex
  //       qResponses = valResponses.map(_(index))
  //       if qResponses.forall(!_.isInvalid)
  //       answer <- wqa.answer :: qResponses.flatMap(_.getAnswer).map(_.indices)
  //     } yield WordedQAPair(wqa.wordIndex, wqa.question, answer)
  //   }
  //   qas
  // }

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

  def getExternalVocabulary(id: SentenceId, qas: List[SourcedQA]) = {
    val tokens = getTokensForId(id)
    qas.flatMap { sqa =>
      val qTokens = tokenize(sqa.question.toLowerCase).toVector
      qTokens.indices
        .filterNot(getAlignedQuestionIndices(tokens, qTokens))
        .map(qTokens.apply _)
    }.toList
  }

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

  def sampleQAs(shuffleRand: util.Random, proportionQAsToSample: Double = 0.2, sentenceToQAs: Map[SentenceId, List[SourcedQA]], n: Int) = {
    def takeQAs(sentences: List[SentenceId], qasDesired: Int): Map[SentenceId, List[SourcedQA]] = sentences match {
      case Nil => Map.empty[SentenceId, List[SourcedQA]]
      case _ if qasDesired <= 0 => Map.empty[SentenceId, List[SourcedQA]]
      case id :: remainingIds =>
        val sentenceQAs = shuffleRand.shuffle(sentenceToQAs(id).toVector)
        val numQAsToSample = math.min(qasDesired, (sentenceQAs.size * proportionQAsToSample).toInt)
        val sampledQAs = sentenceQAs.take(numQAsToSample).toList
        if(sampledQAs.nonEmpty) {
          takeQAs(remainingIds, qasDesired - numQAsToSample) + (id -> sampledQAs)
        } else {
          takeQAs(remainingIds, qasDesired - numQAsToSample)
        }
    }
    val randomlyOrderedSentences = shuffleRand.shuffle(sentenceToQAs.keys.toVector).toList
    takeQAs(randomlyOrderedSentences, n)
  }

  // for stable references to QA pairs in manual analysis records
  case class QAPairId(
    prompt: GenerationPrompt,
    workerId: String,
    assignmentId: String,
    qaIndex: Int)

  case class SourcedQA(
    id: QAPairId,
    wqa: WordedQAPair,
    validatorAnswers: List[ValidationAnswer]
  ) {
    def goodValAnswers = validatorAnswers.flatMap(_.getAnswer.map(_.indices))
    def isValid = validatorAnswers.forall(_.isAnswer)
    def isGood = isValid && (questionWords -- Set("much", "many")).contains(questionTokens.head.toLowerCase)

    def question = wqa.question
    def answers = wqa.answer :: goodValAnswers

    val questionTokens = tokenize(wqa.question)
    val questionTaggedTokens = posTag(questionTokens)
  }

  lazy val allQAs = {
    // not sure how exactly to import the `sequence` extension method...
    import scalaz._
    import Scalaz._
    import scalaz.std.list._
    val iter = for {
      (_, promptToAssignmentMap) <- alignedInfos.iterator
      (prompt, assignmentToInfos) <- promptToAssignmentMap.iterator
      (assignment, infos) <- assignmentToInfos.iterator
      answersByQA = infos.flatMap(_.assignments).map(_.response).transpose
      ((wqa, valAnswers), qaIndex) <- assignment.response.zip(answersByQA).zipWithIndex
    } yield SourcedQA(
      QAPairId(prompt, assignment.workerId, assignment.assignmentId, qaIndex),
      wqa,
      valAnswers)
    iter.toList
  }

  class QAData(
    val allUnfiltered: List[SourcedQA],
    val idToQAUnfiltered: Map[QAPairId, SourcedQA],
    val sentenceToQAsUnfiltered: Map[SentenceId, List[SourcedQA]]
  ) {
    lazy val all = allUnfiltered.filter(_.isGood)
    lazy val idToQA = idToQAUnfiltered.filter(x => x._2.isGood)
    lazy val sentenceToQAs = sentenceToQAsUnfiltered.flatMap { case (id, qas) =>
      val newQAs = qas.filter(_.isGood)
      Some(id -> newQAs).filter(const(newQAs.nonEmpty))
    }

    def this(_all: List[SourcedQA]) = this(
      _all,
      _all.map(sqa => sqa.id -> sqa).toMap,
      _all.groupBy(_.id.prompt.id))

    def filterBySentence(p: SentenceId => Boolean) = new QAData(
      allUnfiltered.filter(sqa => p(sqa.id.prompt.id)),
      idToQAUnfiltered.filter(x => p(x._1.prompt.id)),
      sentenceToQAsUnfiltered.filter(x => p(x._1)))

    def filterByQA(p: SourcedQA => Boolean) = new QAData(
      allUnfiltered.filter(p),
      idToQAUnfiltered.filter(x => p(x._2)),
      sentenceToQAsUnfiltered.flatMap { case (id, qas) =>
        val newQAs = qas.filter(p)
        Some(id -> newQAs).filter(const(newQAs.nonEmpty))
      })
  }

  class CoordinationAnalysis[SID <: SentenceId](data: QAData) {
    val conjunctions = Set("and", "or").map(_.lowerCase)
    lazy val qasWithCoordination = data.sentenceToQAs.flatMap {
      case (id, qas) =>
        val newQAs = qas.filter(
          sqa => sqa.questionTokens
            .map(_.lowerCase).toSet
            .intersect(conjunctions)
            .nonEmpty)
          (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
    }

    lazy val qasWithCoordWh = qasWithCoordination.flatMap {
      case (id, qas) =>
        val newQAs = qas.filter(
          sqa => sqa.questionTokens
            .map(_.lowerCase)
            .sliding(3)
            .filter(
            w => (w.size > 2) && conjunctions.contains(w(1)) &&
              (questionWords.contains(w(0)) || questionWords.contains(w(2))))
            .nonEmpty)
          (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
    }

    lazy val totalNumQAs = data.sentenceToQAs.flatMap(_._2).size
    lazy val numQAsWithCoord = qasWithCoordination.flatMap(_._2).size
    lazy val numQAsWithCoordWh = qasWithCoordWh.flatMap(_._2).size

    lazy val report = f"""
QAs with and/or: $numQAsWithCoord%d (${numQAsWithCoord * 100.0 / totalNumQAs}%.2f%%)
QAs with and/or beside a WH: $numQAsWithCoordWh%d (${numQAsWithCoordWh * 100.0 / numQAsWithCoord}%.2f%%)
""".trim
  }

  def sampleQAPairs(sqas: List[SourcedQA], n: Int = 1) = {
    val qasByPrompt = sqas.groupBy(_.id.prompt)
    val qas = qasByPrompt.values.flatMap { promptQAs =>
      val qasByAssignment = promptQAs.groupBy(_.id.assignmentId)
      val sample = util.Random.shuffle(qasByAssignment.keys.toVector).take(n)
      for {
        assignmentId <- sample
        sqa <- qasByAssignment(assignmentId)
      } yield sqa
    }
    qas
  }

  object PTBAnalysis {

    object PASAnalysis {

      import mts.datasets.propbank._
      import mts.datasets.nombank._

      case class PredArg(pred: Predicate, arg: ArgumentSpan)
      def getRelevantPredArgs(pas: PredicateArgumentStructure) = pas.arguments
        .map(PredArg(pas.pred, _))
        .filterNot(pa => labelIsIrrelevant(pa.arg.label))
        .filterNot(pa => Inflections.auxiliaryVerbs.contains(pa.pred.head.token.lowerCase))
        .filterNot(pa => pa.arg.words.contains(pa.pred.head))

      // heuristic alignment, based on each single question-answer pair
      def alignToPASSimple(
        words: Vector[String],
        sqas: List[SourcedQA],
        paStructures: List[PredicateArgumentStructure]) = {
        val qas = sqas.map(_.wqa)
        val predArgs = paStructures.flatMap(getRelevantPredArgs)
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
        // val numPredictions = alignedPAs.filter(_ == None).size// + pasCovered.size // TODO why was I adding this?
        // because I use qas.size below, I should be consistent here. don't really care about precision anyway...
        val numPredictions = qas.size

        val sentenceString = TextRendering.renderSentence(words)
        val missedDeps = predArgs.filterNot(pasCovered).mkString("\n")
        // println(s"\n\n$sentenceString\nMissed deps:\n$missedDeps")

        PrecisionRecall(
          numPredicted = numPredictions,
          numGold = predArgs.size,
          numCorrect = pasCovered.size,
          numCovered = pasCovered.size
        )
      }

      case class PASAlignment(
        allPAs: List[PredArg],
        alignedQAs: Map[SourcedQA, Option[PredArg]],
        stats: PrecisionRecall) {
        def coveredDeps = alignedQAs.values.flatten.toSet
        def missedDeps = {
          val covered = coveredDeps
          allPAs.filterNot(covered)
        }
      }

      def alignToPASSmarter(
        tokens: Vector[String],
        qas: List[SourcedQA],
        paStructures: List[PredicateArgumentStructure]): PASAlignment = {

        // println(s"Sentence:\n${TextRendering.renderSentence(tokens)}")
        def breakIntoContiguous(s: Set[Int]): List[Set[Int]] = {
          if(s.isEmpty) Nil else {
            val min = s.min
            var max = s.min + 1
            while(s.contains(max)) {
              max = max + 1
            }
            val firstInterval = (min until max).toSet
            firstInterval :: breakIntoContiguous(s -- firstInterval)
          }
        }
        // println(s"Questions:\n${qas.map(_.question).mkString("\n")}")
        val allContiguousSpans = qas.flatMap { sqa =>
          val qSpan = getWordsInQuestion(tokens, sqa.question)
          (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
        }
        // println(s"Contiguous spans:\n${allContiguousSpans.map(TextRendering.renderSpan(tokens, _)).mkString("\n")}")
        val minimalContiguousSpans = allContiguousSpans.filter(span =>
          !allContiguousSpans.exists(otherSpan =>
            otherSpan.subsetOf(span) && !span.subsetOf(otherSpan)
          )
        ).toSet
        val minimalSpanQuestionAppearanceCounts = Scorer[Set[Int], Int](
          qas.flatMap { qa =>
            val qSpan = getWordsInQuestion(tokens, qa.question)
            breakIntoContiguous(qSpan).filter(minimalContiguousSpans.contains)
          }
        )
        // println(s"Minimal contiguous spans:\n${minimalContiguousSpans.map(TextRendering.renderSpan(tokens, _)).mkString("\n")}")
        val minimalSpanAllAppearanceCounts = Scorer[Set[Int], Int](
          allContiguousSpans.filter(minimalContiguousSpans.contains)
        )
        val spansByPredicateness = {
          val spanVec = minimalContiguousSpans.toVector
          spanVec.zip(spanVec.map(s => minimalSpanQuestionAppearanceCounts(s) / minimalSpanAllAppearanceCounts(s)))
            .sortBy(-_._2)
            .map(_._1)
        }
        // println(s"Spans by salience:\n${spansByPredicateness.map(TextRendering.renderSpan(tokens, _)).mkString("\n")}")

        val allPredArgs = paStructures.flatMap(getRelevantPredArgs)
        val alignedQAs = qas.map { sqa =>
          // println(s"QA Pair:\t${sqa.question}\t${sqa.answers.map(TextRendering.renderSpan(tokens, _))}")
          val questionWords = getWordsInQuestion(tokens, sqa.question)
          val questionPAs = for {
            qNode <- spansByPredicateness.filter(_.subsetOf(questionWords))
            pa @ PredArg(pred, arg) <- allPredArgs
            .sortBy { pa =>
              val argSpan = pa.arg.words.map(_.index).toSet
              -1.0 * sqa.answers.map(a => a.intersect(argSpan).size.toDouble / a.union(argSpan).size).mean
            }
            if qNode.contains(pred.head.index)
            argSpan = arg.words.map(_.index).toSet
            if(sqa.answers.filter(a => a.intersect(argSpan).nonEmpty)).size >= 1
          } yield pa
          val predArgOpt = questionPAs.headOption.orElse {
            val answerPAs = for {
              qNode <- spansByPredicateness.filter(_.subsetOf(questionWords))
              pa @ PredArg(pred, arg) <- allPredArgs
              argSpan = arg.words.map(_.index).toSet
              if qNode.subsetOf(argSpan)
              if(sqa.answers.filter(a => a.contains(pred.head.index))).size > 1
            } yield pa
            answerPAs.headOption
          }
          // println(s"PredArg alignment: $predArgOpt")
          sqa -> predArgOpt
        }.toMap
        val numQAsAligned = alignedQAs.values.flatten.size
        val pasCovered = alignedQAs.values.flatten.toSet
        val numPredictions = qas.size
        // println(s"PAs covered: $pasCovered")

        val missedDeps = allPredArgs.filterNot(pasCovered)

        val pr = PrecisionRecall(
          numPredicted = numPredictions,
          numGold = allPredArgs.size,
          numCorrect = numQAsAligned,
          numCovered = pasCovered.size)

        PASAlignment(allPredArgs, alignedQAs, pr)
      }

      def alignToPAS(
        tokens: Vector[String],
        qas: List[SourcedQA],
        paStructures: List[PredicateArgumentStructure]
      ) = alignToPASSmarter(tokens, qas, paStructures)

      import mts.datasets.propbank._

      def propBankPR(path: PropBankSentencePath, tokens: Vector[String], qas: List[SourcedQA]) = {
        val pbSentence = FileManager.getPropBankSentence(path).get
        val paStructures = pbSentence.predicateArgumentStructures
        alignToPAS(tokens, qas, paStructures)
      }

      lazy val numPropBankSentences = {
        val pbPaths = for {
          PTBSentenceId(path) <- ptbData.sentenceToQAs.keys.iterator
          pbPath <- ptbToPropBankSentencePath(path).iterator
        } yield pbPath
        pbPaths.toSet.size
      }

      def allPropBankPRs(n: Int = 1) = {
        val res = for {
          (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
          pbPath <- ptbToPropBankSentencePath(path).iterator
          tokens = getTokensForId(id)
          sampledQAs = sampleQAPairs(qas, n)
        } yield propBankPR(pbPath, tokens, sampledQAs.toList).stats
        res.toList
      }

      lazy val pbRecalls = (1 to 5).map(i => List.fill(6 - i)(allPropBankPRs(i).reduce(_ aggregate _).recall))
      lazy val pbRecallDists = pbRecalls.map(r => (r.mean, r.stdevSample))
      lazy val pbRecallReport = s"PropBank:\nNumber of sentences: $numPropBankSentences\n" + pbRecallDists.zip(1 to 5)
        .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
        .mkString("\n")

      import mts.datasets.nombank._

      def nomBankPR(path: PTBSentencePath, tokens: Vector[String], qas: List[SourcedQA]) = {
        val pas = FileManager.getNomBankPredArgStructuresReindexed(path).get
        alignToPAS(tokens, qas, pas)
      }

      def allNomBankPRs(n: Int = 1) = {
        val res = for {
          (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
          tokens = getTokensForId(id)
          sampledQAs = sampleQAPairs(qas, n)
        } yield nomBankPR(path, tokens, sampledQAs.toList).stats
        res.toList
      }.toList

      lazy val numNomBankSentences = ptbData.sentenceToQAs.keys.size

      lazy val nbRecalls = (1 to 5).map(i => List.fill(6 - i)(allNomBankPRs(i).reduce(_ aggregate _).recall))
      lazy val nbRecallDists = nbRecalls.map(r => (r.mean, r.stdevSample))
      lazy val nbRecallReport = s"NomBank:\nNumber of sentences: $numNomBankSentences\n" + nbRecallDists.zip(1 to 5)
        .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
        .mkString("\n")

      import mts.datasets.qasrl._
      val qasrl = FileManager.getQASRL.get

      // assumes path is stored
      def qasrlPR(path: PTBSentencePath, tokens: Vector[String], qas: List[SourcedQA]) = {
        val qasrlSentence = qasrl(path)
        alignToPAS(tokens, qas, qasrlSentence.predicateArgumentStructures)
      }

      lazy val numQASRLSentences = ptbData.sentenceToQAs.keys
        .collect { case PTBSentenceId(path) => path }
        .filter(qasrl.keySet.contains)
        .size

      def allQASRLPRs(n: Int = 1) = {
        val res = for {
          (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
          if qasrl.keySet.contains(path)
          tokens = getTokensForId(id)
          sampledQAs = sampleQAPairs(qas, n)
        } yield qasrlPR(path, tokens, sampledQAs.toList).stats
        res.toList
      }

      lazy val qasrlRecalls = (1 to 5).map(i => List.fill(6 - i)(allQASRLPRs(i).reduce(_ aggregate _).recall))
      lazy val qasrlRecallDists = qasrlRecalls.map(r => (r.mean, r.stdevSample))
      lazy val qasrlRecallReport = s"QA-SRL:\nNumber of sentences: $numQASRLSentences\n" + qasrlRecallDists.zip(1 to 5)
        .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
        .mkString("\n")

      def writeMissedDeps = {
        val sb = new StringBuilder
        val shuffleRand = new util.Random(821569L)
        val shuffledSentences = shuffleRand.shuffle(ptbData.sentenceToQAs.keys.toVector)
        for (id @ PTBSentenceId(path) <- shuffledSentences; if qasrl.keySet.contains(path)) {
          val qas = ptbData.sentenceToQAs(id)
          val tokens = getTokensForId(id)

          val pbAlignmentOpt = ptbToPropBankSentencePath(path).map(propBankPR(_, tokens, qas))
          val nbAlignment = nomBankPR(path, tokens, qas)
          val qasrlAlignment = qasrlPR(path, tokens, qas)

          def addPA(pa: PredArg): Unit = pa match { case PredArg(pred, arg) =>
            sb.append(s"\t${pred.head.token} (${pred.head.index}) --" + arg.label + "-> ")
            sb.append(TextRendering.renderSentence(arg.words.map(_.token)) + "\n")

            // find relevant QA pairs
            val relevantQAs = qas.filter { qa =>
              val qWordsFromSentence = getWordsInQuestion(tokens, qa.question)
              val allQAIndices = qWordsFromSentence ++ qa.answers.reduce(_ union _)
              val relevantToPred = (
                allQAIndices.contains(pred.head.index) ||
                  inflections.getAllForms(pred.head.token.lowerCase).map(_.toString)
                  .exists(qa.question.toLowerCase.contains))
              val relevantToArg = (
                allQAIndices.intersect(arg.words.map(_.index).toSet).nonEmpty ||
                  qa.question.toLowerCase.contains(
                    TextRendering.renderSpan(tokens, arg.words.map(_.index).toSet).toLowerCase
                  ))
              relevantToPred & relevantToArg
            }

            relevantQAs.foreach { qa =>
              val answers = qa.answers.map(TextRendering.renderSpan(tokens, _)).distinct.mkString(" / ")
              sb.append(s"|\t${qa.question}\t$answers\t")
              pbAlignmentOpt.flatMap(_.alignedQAs(qa)) match {
                case None => sb.append("No PB alignment")
                case Some(PredArg(p, a)) =>
                  sb.append(s"${p.head.token} (${p.head.index}) --" + a.label + "-> ")
                  sb.append(TextRendering.renderSentence(a.words.map(_.token)))
              }
              sb.append("\t")
              nbAlignment.alignedQAs(qa) match {
                case None => sb.append("No NB alignment")
                case Some(PredArg(p, a)) =>
                  sb.append(s"${p.head.token} (${p.head.index}) --" + a.label + "-> ")
                  sb.append(TextRendering.renderSentence(a.words.map(_.token)))
              }
              sb.append("\n")
            }
          }

          if(pbAlignmentOpt.fold(false)(_.missedDeps.nonEmpty) || nbAlignment.missedDeps.nonEmpty || qasrlAlignment.missedDeps.nonEmpty) {
            sb.append("==\t" + TextRendering.renderSentence(tokens) + "\n")
            pbAlignmentOpt.fold[Unit](sb.append("--\tNo PropBank data\n")) { pbAlignment =>
              sb.append("--\tPropBank dependencies missed:\n")
              pbAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
            }
            sb.append("--\tNomBank dependencies missed:\n")
            nbAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
            sb.append("--\tQA-SRL dependencies missed:\n")
            qasrlAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
          }
        }
        val fileString = sb.toString
        // FileManager.saveDataFile(experimentName, "pbnb-missed-deps.tsv", fileString)
        FileManager.saveDataFile(experimentName, "qasrl-missed-deps.tsv", fileString)
      }
    }

    // qasrl comparison too

    // Constituency

    class ConstituencyAnalysis(theseQAs: Map[SentenceId, List[SourcedQA]]) {
      // pct of answers that are spans
      sealed trait SpanClassification {
        def coordSibling: Boolean
      }
      case class SingleWord(coordSibling: Boolean = false) extends SpanClassification
      case class SubNPSpan(coordSibling: Boolean = false) extends SpanClassification
      case class ExactSpan(symbol: String, coordSibling: Boolean = false) extends SpanClassification
      case object NonSpan extends SpanClassification {
        override def coordSibling = false
      }

      def getSubtrees(tree: SyntaxTree, coordSibling: Boolean): List[(SyntaxTree, Boolean)] = tree match {
        case leaf @ SyntaxTreeLeaf(_) => List((leaf, coordSibling))
        case node @ SyntaxTreeNode(_, children) =>
          val coordChild = children.collect {
            case SyntaxTreeLeaf(word) => word.pos
          }.contains("CC")
          (node, coordSibling) :: children
            .map((_, coordChild))
            .flatMap(Function.tupled(getSubtrees))
      }

      def classifySpan(subtrees: List[(SyntaxTree, Boolean)], span: Set[Int]): SpanClassification = {
        val beginSubtrees = subtrees.filter(_._1.beginIndex == span.min).toSet
        val endSubtrees = subtrees.filter(_._1.endIndex == span.max).toSet

        (beginSubtrees intersect endSubtrees).headOption.map {
          case (SyntaxTreeLeaf(w), coordSibling) =>
            SingleWord(coordSibling)
          case (SyntaxTreeNode(label, _), coordSibling) => ExactSpan(label, coordSibling)
        }.orElse {
          subtrees.collect { case (node @ SyntaxTreeNode(_, _), coordSibling) => (node, coordSibling) }
            .filter(pair => pair._1.label.contains("NP") && pair._1.depth == 1)
            .find(pair => pair._1.beginIndex <= span.min && pair._1.endIndex >= span.max)
            .map(p => SubNPSpan(p._2))
        }.getOrElse {
          NonSpan
        }
      }

      lazy val allSpanClassifications = theseQAs.iterator.collect { case (PTBSentenceId(path), sqas) =>
        val sentence = FileManager.getPTBSentence(path).get
        val answerSpans = sqas.flatMap(_.answers)
        val subtrees = getSubtrees(sentence.syntaxTree, false)
        answerSpans.map(classifySpan(subtrees, _))
      }.flatten.toList

      lazy val spanClassificationCounts = Scorer[SpanClassification, Int](allSpanClassifications.iterator)

      lazy val sortedSpanClassifications = spanClassificationCounts.iterator.toVector.sortBy(-_._2)

      lazy val totalNumAnswerSpans = spanClassificationCounts.sum

      def constituencyReport(spanClass: SpanClassification, count: Int) =
        f"${spanClass.toString.takeWhile(_ != '(')}%s\t$count%d\t(${count * 100.0 / totalNumAnswerSpans}%.2f%%)"

      lazy val coordinationConstituencyReport = {
        val coordSiblingCount = spanClassificationCounts.sumIf(_.coordSibling)
        val notCoordSiblingCount = spanClassificationCounts.sumIf(!_.coordSibling)
        def fineGrainedReport(spanClass: SpanClassification) = {
          val count = spanClassificationCounts(spanClass)
          f"$spanClass%s\t$count%d\t(${count * 100.0 / totalNumAnswerSpans}%.2f%%)"
        }
        val overallReport = f"Coordinator as sibling:\t(${pctString(coordSiblingCount, totalNumAnswerSpans)}%s)\n" +
          f"No coordinator as sibling:\t(${pctString(notCoordSiblingCount, totalNumAnswerSpans)}%s)\n"
        val someReports = List(SingleWord(true), SingleWord(false), SubNPSpan(true), SubNPSpan(false), NonSpan).map(fineGrainedReport)
        val exactSpanReports = {
          val exactMatchCoordSiblingCount = spanClassificationCounts.sumIf {
            case ExactSpan(_, cs) => cs
            case _ => false
          }
          val exactMatchNoCoordSiblingCount = spanClassificationCounts.sumIf {
            case ExactSpan(_, cs) => !cs
            case _ => false
          }
          "Coord sibling: " + constituencyReport(ExactSpan("", true), exactMatchCoordSiblingCount) + "\n" +
          "No coord sibling: " + constituencyReport(ExactSpan("", false /* doesn't actually matter*/), exactMatchNoCoordSiblingCount)
        }
        overallReport + someReports.mkString("\n") + "\n" + exactSpanReports
      }

      lazy val fullConstituencyReport = {
        val singleWordCount = spanClassificationCounts.sumIf {
          case SingleWord(_) => true
          case _ => false
        }
        val subNPCount = spanClassificationCounts.sumIf {
          case SubNPSpan(_) => true
          case _ => false
        }
        val exactMatchCount = spanClassificationCounts.sumIf {
          case ExactSpan(_, _) => true
          case _ => false
        }
        val nonSpanCount = spanClassificationCounts(NonSpan)
        "Constituency:\n" +
          constituencyReport(SingleWord(false), singleWordCount) + "\n" +
          constituencyReport(SubNPSpan(false), subNPCount) + "\n" +
          constituencyReport(ExactSpan("", false), exactMatchCount) + "\n" +
          constituencyReport(NonSpan, nonSpanCount)
      }
    }

    lazy val generalConstituencyAnalysis = new ConstituencyAnalysis(ptbData.sentenceToQAs)

    lazy val ptbCoordinationAnalysis = new CoordinationAnalysis(ptbData)
    lazy val coordinationConstituency = new ConstituencyAnalysis(ptbCoordinationAnalysis.qasWithCoordWh)

    lazy val nonStartWhQAs = ptbData.sentenceToQAs.flatMap {
      case (id, qas) =>
        val newQAs = qas.filter(sqa => !questionWords.contains(sqa.questionTokens.head))
        (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
    }
    lazy val nonStartWhConstituencyAnalysis = new ConstituencyAnalysis(nonStartWhQAs)

    lazy val fullReport: String = {
      val pasAlignment = s"PAS alignment:\n" +
        PASAnalysis.pbRecallReport + "\n" +
        PASAnalysis.nbRecallReport + "\n" +
        PASAnalysis.qasrlRecallReport + "\n"
      val genConstituency = "\nGeneral constituency analysis:\n" +
        generalConstituencyAnalysis.fullConstituencyReport + "\n"
      val nonStartWhConstituency = "\nConstituency analysis for non-start WH-words:\n" +
        nonStartWhConstituencyAnalysis.fullConstituencyReport + "\n"
      val coordReport = "\nCoordination analysis:\n" +
        ptbCoordinationAnalysis.report + "\n"
      val coordConstituency = "Constituency analysis for coordinated answers:\n" +
        "Coordination-specific:\n " + coordinationConstituency.coordinationConstituencyReport + "\n"
        coordinationConstituency.fullConstituencyReport + "\n"
      pasAlignment + genConstituency + nonStartWhConstituency + coordReport + coordConstituency
    }

    def makeTSV(thisData: QAData): String = {
      val sb = new StringBuilder
      for((id, sqas) <- thisData.sentenceToQAs.iterator) {
        val sentence = getTokensForId(id)
        sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
        for(sqa <- sqas.iterator) {
          val answerStrings = sqa.answers.map(TextRendering.renderSpan(sentence, _))
          val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
          val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
          sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
          sb.append(sqa.id.prompt.keywords.toVector.sorted.mkString(" ") + "\t")
          sb.append(s"${sqa.id.workerId}\t")
          sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
          sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
          sb.append(sqa.answers.size + "\t")
          sb.append(answerStrings.mkString("\t") + "\t")
          sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
          sb.append("\n")
        }
      }
      sb.toString
    }

    def writePTBTSVs = {
      val trainData = ptbData.filterBySentence(isTrain)
      println("PTB train sentences: " + trainData.sentenceToQAs.size)
      val devData = ptbData.filterBySentence(isDev)
      println("PTB dev sentences: " + devData.sentenceToQAs.size)
      val testData = ptbData.filterBySentence(isTest)
      println("PTB test sentences: " + testData.sentenceToQAs.size)
      val amrData = ptbData.filterBySentence(ptb100ForAMR.contains)
      println("PTB AMR sentences: " + amrData.sentenceToQAs.size)
      FileManager.saveDataFile(experimentName, "ptb-train.tsv", makeTSV(trainData))
      FileManager.saveDataFile(experimentName, "ptb-dev.tsv", makeTSV(devData))
      FileManager.saveDataFile(experimentName, "ptb-test.tsv", makeTSV(testData))
      FileManager.saveDataFile(experimentName, "ptb-amr.tsv", makeTSV(amrData))
    }
  }

  class CompleteAnalysis(data: QAData) {
    object QuestionModeling {
      // def validQAsForNumQuestionWords(p: Int => Boolean) = theseValidQAs.map { case (id, qas) =>
      //   val tokens = getTokensForId(id)
      //   id -> qas.filter { case (wqa, answers) => p(getWordsInQuestion(tokens, wqa.question).size) }
      // }.toMap

      lazy val externalWordCounts = data.sentenceToQAs.iterator.flatMap(
        Function.tupled(getExternalVocabulary)
      ) <| Scorer.apply[String, Int]

      // def externalWordReport(sum: Double)(word: String, count: Int) =
      //   f"$word%s\t$count%d\t${count.toDouble / sum}%.4f"

      // lazy val externalWordReports = externalWordCounts.iterator.toVector
      //   .sortBy(-_._2)
      //   .map(Function.tupled(externalWordReport(externalWordCounts.sum)))

      // lazy val externalNonStopwordCounts = theseValidQAs.iterator.flatMap(
      //   Function.tupled(getExternalVocabulary)
      // ).filterNot(isReallyUninteresting) <| Scorer.apply[String, Int]

      // lazy val externalNonStopwordReports = externalNonStopwordCounts.iterator.toVector
      //   .sortBy(-_._2)
      //   .map(Function.tupled(externalWordReport(externalNonStopwordCounts.sum)))

      // lazy val allValidQuestions = theseValidQAs.map {
      //   case (id, qaMap) => id -> qaMap.keys.map(wqa => posTag(tokenize(wqa.question)))
      // }.toMap

      // lazy val numValidQuestions = allValidQuestions.iterator.map(_._2.size).sum

      // class NGramReport(tokenizedStrings: Iterator[List[String]]) {
      //   lazy val prefixes = tokenizedStrings.flatMap(tokens =>
      //     (tokens ++ List("<End>")).inits.filter(_.nonEmpty)
      //   ) <| Scorer.apply[List[String], Int]

      //   lazy val orderedPrefixes = prefixes.iterator.toVector.sortBy(-_._2)

      //   def prefixReport(prefix: List[String], count: Int) =
      //     f"${prefix.mkString(" ")}%s\t$count%d\t${count.toDouble / numValidQuestions}%.4f"

      //   lazy val orderedPrefixReports = orderedPrefixes.map(Function.tupled(prefixReport))
      // }

      // lazy val questionNGrams = new NGramReport(allValidQuestions.iterator.flatMap(_._2).map(_.map(_.token.toLowerCase)))

      // lazy val collapsedQuestions = allValidQuestions.map {
      //   case (id, questions) => id -> questions.map { q =>
      //     val alignedTokens = getAlignedQuestionIndices(getTokensForId(id), q.map(_.token).toVector)
      //     val collapsedTokens = q.zipWithIndex.foldRight(List.empty[POSTaggedToken]) { case ((posToken, index), acc) =>
      //       if(alignedTokens.contains(index)) {
      //         if(acc.headOption.fold(true)(_.token != "<>")) POSTaggedToken("<>", "<>") :: acc
      //         else acc
      //       } else posToken.copy(token = posToken.token.toLowerCase) :: acc
      //     }
      //     collapsedTokens
      //   }
      // }

      // lazy val collapsedQuestionNGrams = new NGramReport(collapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

      // lazy val auxCollapsedQuestions = collapsedQuestions.map {
      //   case (id, cQuestions) => id -> cQuestions.map { tokens =>
      //     tokens.map { case t =>
      //       if(inflections.isCopulaVerb(t.token.lowerCase)) POSTaggedToken("<be>", "<be>")
      //       else if(Inflections.doVerbs.contains(t.token.lowerCase)) POSTaggedToken("<do>", "<do>")
      //       else t
      //     }
      //   }
      // }

      // lazy val auxCollapsedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

      // lazy val delexicalizedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.pos)))

      // val determiners = Set("the", "a", "this")
      // val pronouns = Set("i", "we", "you", "he", "she", "him", "her", "it", "something", "someone", "they", "them")
      // val kindCats = Set("kind", "type", "types")
      // val whCats = Set("year", "country", "part", "month", "day", "people", "nationality", "city", "place", "thing",
      //                  "group", "event", "time", "number", "man", "things", "language", "person", "album", "position",
      //                  "animal", "years", "state", "size", "color", "score", "percentage", "date", "gender", "countries",
      //                  "direction", "organization", "level", "religion", "profession", "company", "job")
      // val ofCats = Set("name", "title")
      // val howCats = Set("long", "old")

      // def writeTemplatedQuestions(filename: String) = {
      //   val sb = new StringBuilder
      //   for {
      //     (id, qaPairToAnswers) <- theseValidQAs
      //     sentenceTokens = posTag(getTokensForId(id).toList)
      //                            (wqa, answers) <- qaPairToAnswers
      //     (qTokens, qTags) = delexicalizeQuestion(id, wqa.question).unzip
      //     alignments = getQuestionSentenceAlignments(sentenceTokens.toVector.map(_.token), qTokens.toVector)
      //     sQuestionIndices = alignments.map(_._2)
      //     qSentenceIndices = alignments.map(_._1)
      //     answer <- wqa.answer :: answers
      //   } yield {
      //     val sTags = sentenceTokens.zipWithIndex.map { case(POSTaggedToken(_, pos), i) =>
      //       val placementTag = if(sQuestionIndices(i)) "Q" else if(answer(i)) "A" else "O"
      //       s"$placementTag-$pos"
      //     }.mkString(" ")
      //     val line = s"${sentenceTokens.map(_.token).mkString(" ")} ||| $sTags ||| ${qTokens.mkString(" ")} ||| ${qTags.mkString(" ")}\n"
      //     sb.append(line)
      //   }

      //   FileManager.saveDataFile(experimentName, filename, sb.toString)
      // }
      lazy val questionDuplicationCounts = Scorer[Int, Int](data.all.groupBy(_.question).map(_._2.size))

      lazy val (anyWhQuestions, noWhQuestions) = data.all.partition(q => q.questionTokens.map(_.toLowerCase).exists(questionWords.contains))
      lazy val (beginWhQuestions, nonBeginWhQuestions) =
        anyWhQuestions.partition(q => questionWords.contains(q.questionTokens.head.toLowerCase))

      lazy val nonBeginWhWordBreakdown = Scorer[String, Int](
        anyWhQuestions.map(
          _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
        ).iterator
      )
      lazy val nonBeginWhWordBreakdownReport = nonBeginWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
        case (word, count) => s"$word: ${pctString(count, nonBeginWhQuestions.size)}"
      }.mkString("\n")

      lazy val beginWhWordBreakdown = Scorer[String, Int](
        beginWhQuestions.map(
          _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
        ).iterator
      )
      lazy val beginWhWordBreakdownReport = beginWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
        case (word, count) => s"$word: ${pctString(count, beginWhQuestions.size)}"
      }.mkString("\n")

      lazy val anyWhWordBreakdown = Scorer[String, Int](
        anyWhQuestions.map(
          _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
        ).iterator
      )
      lazy val anyWhWordBreakdownReport = anyWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
        case (word, count) => s"$word: ${pctString(count, anyWhQuestions.size)}"
      }.mkString("\n")

      lazy val report = s"""
Number of valid questions: ${data.all.size}
Question duplications:
${histogramString(questionDuplicationCounts)}
Number of questions beginning with a question word: ${pctString(beginWhQuestions.size, data.all.size)}
Number of questions with question word not at beginning: ${pctString(nonBeginWhQuestions.size, data.all.size)}
Number of questions with no question word: ${pctString(noWhQuestions.size, data.all.size)}
First question word breakdown:
$anyWhWordBreakdownReport

Non-beginning word breakdown:
$nonBeginWhWordBreakdownReport

Beginning wh-words:
$beginWhWordBreakdownReport
""".trim
    }

    lazy val coordinationAnalysis = new CoordinationAnalysis(data)

    class RelationAnalysis(extractRelationWords: SourcedQA => List[String]) {
      lazy val relationWordCounts = Scorer.apply[String, Int](data.all.iterator.flatMap(extractRelationWords))

      lazy val questionsWithRelationWords = data.filterByQA(sqa => extractRelationWords(sqa).nonEmpty)

      lazy val questionsByRelation = {
        var res = Map.empty[String, List[SourcedQA]]
        questionsWithRelationWords.all.iterator.foreach { sqa =>
          extractRelationWords(sqa).foreach { w =>
            res = res.updated(w, sqa :: res.get(w).getOrElse(Nil))
          }
        }
        res
      }

      lazy val totalNumRelationWords = relationWordCounts.size
      lazy val totalCountRelationWords = relationWordCounts.sum

      lazy val orderedCountedRelationWords = relationWordCounts.iterator.toVector.sortBy(-_._2)
      lazy val relationWordPrintables = orderedCountedRelationWords.map(p => s"${p._1} ${p._2} (${p._2 * 100.0 / totalCountRelationWords})")

      // lazy val qaSample = sampleQAs(shuffleRand, )
      def computeCoverageAtPercentile(topPercentile: Double) = {
        val numPhrasesIncluded = math.round(totalNumRelationWords * topPercentile).toInt
        val countCovered = orderedCountedRelationWords.take(numPhrasesIncluded).map(_._2).sum
        s"${topPercentile * 100.0}% ($numPhrasesIncluded): ${pctString(countCovered, totalCountRelationWords)}"
      }

      def proportionQAsToSample = 0.2
      // assumes we won't need to sample from all of the sentences
      def sampleQuestions(shuffleRand: util.Random, n: Int) = {
        def takeQAs(sentences: List[SentenceId], qasDesired: Int): Map[SentenceId, List[(List[String], SourcedQA)]] = sentences match {
          case Nil => Map.empty[SentenceId, List[(List[String], SourcedQA)]]
          case _ if qasDesired <= 0 => Map.empty[SentenceId, List[(List[String], SourcedQA)]]
          case id :: remainingIds =>
            val sentenceQAs = shuffleRand.shuffle(data.sentenceToQAs(id).toVector)
            val numQAsToSample = math.min(qasDesired, (sentenceQAs.size * proportionQAsToSample).toInt)
            val sampledQAs = sentenceQAs.map(sqa => (extractRelationWords(sqa), sqa))
              .filter(_._1.nonEmpty)
              .take(numQAsToSample)
              .toList
            if(sampledQAs.nonEmpty) {
              takeQAs(remainingIds, qasDesired - numQAsToSample) + (id -> sampledQAs)
            } else {
              takeQAs(remainingIds, qasDesired - numQAsToSample)
            }
        }
        val randomlyOrderedSentences = shuffleRand.shuffle(data.sentenceToQAs.keys.toVector).toList
        takeQAs(randomlyOrderedSentences, n)
      }

      lazy val report = s"""
Number of questions with relation phrase: ${pctString(questionsWithRelationWords.all.size, data.all.size)}
Number of relation phrases: $totalNumRelationWords
Number of relation phrase instances: $totalCountRelationWords
Relation phrase question coverages at percentiles:
${computeCoverageAtPercentile(0.001)}
${computeCoverageAtPercentile(0.01)}
${computeCoverageAtPercentile(0.05)}
${computeCoverageAtPercentile(0.10)}
${computeCoverageAtPercentile(0.25)}
${computeCoverageAtPercentile(0.50)}
${computeCoverageAtPercentile(0.90)}

Sample phrases:
${relationWordPrintables.take(100).mkString("\n")}
"""
    }

    val whDetWords = Set("what", "how", "which").map(_.lowerCase)
    case class PhraseState(curPhrase: List[LowerCaseString], phrases: List[List[LowerCaseString]]) {
      def finish(include: Boolean) = PhraseState(Nil, if(include && curPhrase.nonEmpty) curPhrase :: phrases else phrases)
      def extend(token: String) = this.copy(curPhrase = token.lowerCase :: this.curPhrase)

      def hasQWord: Boolean = curPhrase.exists(whDetWords.contains)
      def hasNonQWord: Boolean = curPhrase.exists(!whDetWords.contains(_))
    }

    val interestingWords = Set("first", "last", "second", "third", "fourth")
    def isInteresting(t: String) = !isReallyUninteresting(t) || interestingWords.contains(t)

    // def getAllRelationWords(sqa: SourcedQA) = {
    //   val tokens = getTokensForId(sqa.id.prompt.id)
    //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
    //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
    //   qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, acc) =>
    //     val token = qTokens(index)
    //     if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
    //       acc.extend(token.lowerCase)
    //     } else acc.finish(true)
    //   }.phrases.map(_.mkString(" "))
    // }
    // def getWhRelationPhrases(sqa: SourcedQA) = {
    //   val tokens = getTokensForId(sqa.id.prompt.id)
    //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
    //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
    //   val finalPhraseState = qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, phraseState) =>
    //     val token = qTokens(index)
    //     if(whDetWords.contains(token.lowerCase)) {
    //       phraseState.extend(token)
    //     } else if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
    //       if(phraseState.hasQWord) phraseState.finish(phraseState.hasNonQWord).extend(token)
    //       else phraseState.extend(token)
    //     } else {
    //       phraseState.finish(phraseState.hasQWord && phraseState.hasNonQWord)
    //     }
    //   }
    //   finalPhraseState.finish(finalPhraseState.hasQWord && finalPhraseState.hasNonQWord).phrases.map(_.mkString(" "))
    // }
    // def getNonWhRelationWords(sqa: SourcedQA) = {
    //   val tokens = getTokensForId(sqa.id.prompt.id)
    //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
    //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
    //   qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, acc) =>
    //     val token = qTokens(index)
    //     if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
    //       acc.extend(token.lowerCase)
    //     } else acc.finish(!whDetWords.contains(token.lowerCase))
    //   }.phrases.map(_.mkString(" "))
    // }

    def getExternalPhrases(sqa: SourcedQA) = {
      val tokens = getTokensForId(sqa.id.prompt.id)
      val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
      val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
      val finalPhraseState = qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, phraseState) =>
        val token = qTokens(index)
        if(whDetWords.contains(token.lowerCase)) {
          phraseState.extend(token)
        } else if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
          if(phraseState.hasQWord) phraseState.finish(phraseState.hasNonQWord).extend(token)
          else phraseState.extend(token)
        } else {
          phraseState.finish(phraseState.hasNonQWord)
        }
      }
      finalPhraseState.finish(finalPhraseState.hasNonQWord).phrases
    }
    def getAllExternalPhrases(sqa: SourcedQA) = {
      getExternalPhrases(sqa).map(_.mkString(" "))
    }
    def getWhExternalPhrases(sqa: SourcedQA) = {
      getExternalPhrases(sqa).filter(_.exists(t => whDetWords.contains(t))).map(_.mkString(" "))
    }
    def getNonWhExternalPhrases(sqa: SourcedQA) = {
      getExternalPhrases(sqa).filter(!_.exists(t => whDetWords.contains(t))).map(_.mkString(" "))
    }

    val allRelationWordAnalysis = new RelationAnalysis(getAllExternalPhrases)
    val whRelationWordAnalysis = new RelationAnalysis(getWhExternalPhrases)
    val nonWhRelationWordAnalysis = new RelationAnalysis(getNonWhExternalPhrases)

    // TODO put this in a more reasonable place
    val lowerQWords = questionWords.map(_.lowerCase)

    object DisagreementAnalysis {
      // // explicit coref: delexicalized question of form
      // val explicitCorefRegex = """WP <be> (?:\*?DT )?(?:\*?(?:NN|NNS|NNP|NNPS|PDT|PRP|FW)).*""".r
      // lazy val explicitCorefQAs = validQAs.flatMap {
      //   case (id, qas) =>
      //     val newQAs = qas.filter { case (wqa, answers) =>
      //       val template = delexicalizeQuestion(id, wqa.question).map(_._2 + " ").mkString
      //       template match {
      //         case explicitCorefRegex(_*) => true
      //         case _ => false
      //       }
      //     }
      //       (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
      // }.toMap

      // implicit coref: nonoverlapping answers to same question
      lazy val disagreementQAs = data.filterByQA { sqa =>
        sqa.answers.reduce(_ intersect _).isEmpty
      }

      // lazy val sameAnswerQs = validQAs.flatMap {
      //   case (id, qas) =>
      //     val groupedQs = qas.iterator.map { case (wqa, answers) =>
      //       (wqa, answers) -> answers.reduce(_ intersect _)
      //     }.filter(_._2.nonEmpty).toVector.groupBy(_._2).map {
      //       case (answer, qPairs) => answer -> qPairs.map(_._1).toSet.toVector
      //     }.filter(_._2.size > 1).toMap
      //     (id -> groupedQs).onlyIf(const(groupedQs.nonEmpty))
      // }.toMap

      lazy val report = s"""
Number of questions with disagreeing answers: ${pctString(disagreementQAs.all.size, data.all.size)}
""".trim
    }

    object ValidationStats {
      case class SentenceInfo(assignmentInfos: List[AssignmentInfo]) {
        def sqas = assignmentInfos.flatMap(_.sqas)
        def id = sqas.head.id.prompt.id
        def numQAs = sqas.size
        def numValidQAs = sqas.filter(_.isValid).size
        def numGoodQAs = sqas.filter(_.isGood).size
        def cost = assignmentInfos.map(_.totalReward.toDouble).sum
        def costPerToken = cost / getTokensForId(id).size
        def numKeywords = assignmentInfos.map(_.numKeywords).sum
      }

      case class AssignmentInfo(keywordInfos: List[KeywordInfo]) {
        def sqas = keywordInfos.flatMap(_.sqas)
        def prompt = sqas.head.id.prompt
        def numQAs = sqas.size
        def numValidForBonus = math.round(
          sqas.map(_.validatorAnswers.filter(_.isAnswer).size).mean - 0.01
        ).toInt
        def numValidQAs = sqas.filter(_.isValid).size
        def numGoodQAs = sqas.filter(_.isGood).size
        def genReward = generationReward + (1 to (numValidQAs - prompt.keywords.size)).map(bonusFor).sum
        def valReward = validationReward + (validationBonusPerQuestion * math.max(0, numQAs - 4))
        def totalReward = genReward + valReward
        def numKeywords = keywordInfos.size
      }
      case class KeywordInfo(sqas: List[SourcedQA]) {
        def prompt = sqas.head.id.prompt
        def keywords = prompt.keywords
        def numQAs = sqas.size
        def numValidQAs = sqas.filter(_.isValid).size
        def numGoodQAs = sqas.filter(_.isGood).size
      }

      lazy val sentenceInfos = data.sentenceToQAsUnfiltered.values.iterator.map { sentenceSQAs =>
        SentenceInfo(
          sentenceSQAs.groupBy(_.id.assignmentId).values.map { assignmentSQAs =>
            AssignmentInfo(
              assignmentSQAs.groupBy(_.wqa.wordIndex).values.map { sqas =>
                KeywordInfo(sqas.toList)
              }.toList
            )
          }.toList
        )
      }.toList

      lazy val assignmentInfos = sentenceInfos.flatMap(_.assignmentInfos)
      lazy val keywordInfos = assignmentInfos.flatMap(_.keywordInfos)

      sealed trait AgreementClass
      case object BothInvalid extends AgreementClass
      case object OneInvalid extends AgreementClass
      case object BothRedundant extends AgreementClass
      case object OneRedundant extends AgreementClass
      case object BothWithOriginal extends AgreementClass
      case object BothButNotOriginal extends AgreementClass
      case object OneWithOriginal extends AgreementClass
      case object NoIntersection extends AgreementClass
      lazy val agreementClasses = data.allUnfiltered.map(sqa => (sqa, sqa.validatorAnswers)).collect {
        case (sqa, List(InvalidQuestion, InvalidQuestion)) => BothInvalid
        case (sqa, List(InvalidQuestion, _)) => OneInvalid
        case (sqa, List(_, InvalidQuestion)) => OneInvalid
        case (sqa, List(Redundant(_), Redundant(_))) => BothRedundant
        case (sqa, List(_, Redundant(_))) => OneRedundant
        case (sqa, List(Redundant(_), _)) => OneRedundant
        case (sqa, List(Answer(a1), Answer(a2))) if (sqa.wqa.answer :: a1 :: a2 :: Nil).reduce(_ intersect _).nonEmpty => BothWithOriginal
        case (sqa, List(Answer(a1), Answer(a2))) if (a1 :: a2 :: Nil).reduce(_ intersect _).nonEmpty => BothButNotOriginal
        case (sqa, (List(Answer(a1), Answer(a2)))) if (
          (a1 intersect sqa.wqa.answer).nonEmpty || (a2 intersect sqa.wqa.answer).nonEmpty
        ) => OneWithOriginal
        case (sqa, List(Answer(a1), Answer(a2))) if (
          (sqa.wqa.answer ++ a1 ++ a2).size == (sqa.wqa.answer.size + a1.size + a2.size)
        ) => NoIntersection
      }

      lazy val agClassCounts = Scorer[AgreementClass, Int](agreementClasses.iterator)

      lazy val agClassHist: String = {
        val keys = Vector(
          BothInvalid, OneInvalid, BothRedundant, OneRedundant,
          BothWithOriginal, BothButNotOriginal, OneWithOriginal, NoIntersection
        )
        val max = agClassCounts.max
        val scaleMax = 50.0
        val scaleFactor = scaleMax / max
        def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
        def pounds(n: Int) = "#" * n
        keys.zip(keys.map(agClassCounts.apply))
          .map { case (c, n) => f"$c%18s |${pounds(scale(n))}%s $n%d"}
          .mkString("\n")
      }


      lazy val report = f"""
Aggregate stats:
Number of questions: ${sentenceInfos.map(_.numQAs).sum}%s
Number of valid questions: ${pctString(sentenceInfos.map(_.numValidQAs).sum, sentenceInfos.map(_.numQAs).sum)}%s
Number of good questions: ${pctString(sentenceInfos.map(_.numGoodQAs).sum, sentenceInfos.map(_.numValidQAs).sum)}%s
Validator agreement class counts:
$agClassHist

Sentences:
Number of sentences: ${sentenceInfos.size}%s
Number of sentences with good QAs: ${pctString(data.sentenceToQAs.size, sentenceInfos.size)}%s
Number of keywords per sentence: ${sentenceInfos.map(_.numKeywords).sum}%s
Sentence costs: ${noSumDistString(sentenceInfos.map(_.cost))}%s
Sentence cost per token: ${noSumDistString(sentenceInfos.map(_.costPerToken))}%s
Number of questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numQAs))}%s
Number of valid questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numValidQAs))}%s
Number of good questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numGoodQAs))}%s

Assignments:
Number of assignments: ${assignmentInfos.size}%s
Assignment costs: ${noSumDistString(assignmentInfos.map(_.totalReward))}%s
Number of questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numQAs))}%s
Number of valid questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numValidQAs))}%s
Number of good questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numGoodQAs))}%s

Keywords:
Number of keywords: ${keywordInfos.size}%s
Number of questions per keyword: ${noSumDistString(keywordInfos.map(_.numQAs))}%s
Number of valid questions per keyword: ${noSumDistString(keywordInfos.map(_.numValidQAs))}%s
Number of good questions per keyword: ${noSumDistString(keywordInfos.map(_.numGoodQAs))}%s
Number of keywords missing: ${pctString(keywordInfos.filter(_.numValidQAs == 0).size, keywordInfos.size)}%s
""".trim
    }

    def report: String = {
      val sep = "\n" + ("=" * 20) + "\n"
      println("Computing report...")
      sep + "Question modeling:\n" + QuestionModeling.report + sep +
        sep + "External word stats:\n" + allRelationWordAnalysis.report + sep +
        sep + "Classifier relation word stats:\n" + whRelationWordAnalysis.report + sep +
        sep + "Non-classifier relation word stats:\n" + nonWhRelationWordAnalysis.report + sep +
        sep + "Annotator disagreement:\n" + DisagreementAnalysis.report + sep
    }

    def topRelationPhrases(n: Int): String =
      allRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")
    def topWhRelationPhrases(n: Int): String =
      whRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")
    def topNonWhRelationPhrases(n: Int): String =
      nonWhRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")

    // used to provide resources for submission
    def printSentencesToAnnotateNICE = {
      import data._
      import scalaz._
      import Scalaz._
      import scala.language.higherKinds
      type PrintingState[A] = State[List[String], A]
      type Printing[A] = ListT[PrintingState, A]
      def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
      def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))

      def niceTSV(allQAs: List[(SentenceId, List[SourcedQA])]) = {
        val printer = for {
          _ <- append(s"Below are all question-answer pairs we collected for a sample of ${allQAs.size} sentences. (tab-separated; best viewed as a spreadsheet)")
          (id, qas) <- iter(allQAs)
          sentenceTokens = getTokensForId(id)
          _ <- append("\n" + TextRendering.renderSentence(sentenceTokens) + "\n")
          sqa <- iter(qas)
          _ <- append(s"${sqa.question}\t")
          _ <- append(sqa.answers.map(a => TextRendering.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
        } yield ()
        printer.run.exec(Nil).reverse.mkString
      }

      // FileManager.saveDataFile(
      //   experimentName,
      //   "wh-relationQs.tsv",
      //   relationTSV(whRelationWordAnalysis.sampleQuestions(new util.Random(7654326L), 1000)))
      // FileManager.saveDataFile(
      //   experimentName,
      //   "nonWh-relationQs.tsv",
      //   relationTSV(nonWhRelationWordAnalysis.sampleQuestions(new util.Random(35265419L), 1000)))

      // FileManager.saveDataFile(
      //   experimentName,
      //   "disagreementQs.tsv",
      //   niceTSV(sampleQAs(new util.Random(356127158L), 1.0, DisagreementAnalysis.disagreementQAs.sentenceToQAs, 1000)))

      val fullSample = sampleQAs(new util.Random(23536289L), 1.0, data.sentenceToQAs, 500).toList
      val trainSample = fullSample.filter(p => isTrain(p._1)).take(50)
      val devSample = fullSample.filter(p => isDev(p._1)).take(50)

      FileManager.saveDataFile(
        experimentName,
        "train-sample.tsv",
        niceTSV(trainSample))

      FileManager.saveDataFile(
        experimentName,
        "dev-sample.tsv",
        niceTSV(devSample))

      // FileManager.saveDataFile(
      //   experimentName,
      //   "dev-sample.tsv",
      //   niceTSV(
      //     sampleQAs(
      //       new util.Random(23536289L),
      //       1.0,
      //       data.sentenceToQAs.filter(p => isDev(p._1)),
      //       50)))
    }

    def printSentencesToAnnotate = {
      import data._
      import scalaz._
      import Scalaz._
      import scala.language.higherKinds
      type PrintingState[A] = State[List[String], A]
      type Printing[A] = ListT[PrintingState, A]
      def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
      def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))

      def relationTSV(allQAs: Map[SentenceId, List[(List[String], SourcedQA)]]) = {
        val printer = for {
          _ <- append("\nCode\tPhrases\tQuestion\tOriginal Answer\tValidator Answers\n")
          (id, qas) <- iter(allQAs.toList)
          sentenceTokens = getTokensForId(id)
          _ <- append("==\t" + TextRendering.renderSentence(sentenceTokens) + "\n")
          (phrases, (sqa)) <- iter(qas.toList)
          _ <- append("\t" + phrases.mkString("; ") + s"\t${sqa.question}\t")
          _ <- append(sqa.answers.map(a => TextRendering.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
        } yield ()
        printer.run.exec(Nil).reverse.mkString
      }

      def generalTSV(allQAs: Map[SentenceId, List[SourcedQA]]) = {
        val printer = for {
          _ <- append("\nCode\tQuestion\tOriginal Answer\tValidator Answers\n")
          (id, qas) <- iter(allQAs.toList)
          sentenceTokens = getTokensForId(id)
          _ <- append("==\t" + TextRendering.renderSentence(sentenceTokens) + "\n")
          sqa <- iter(qas)
          _ <- append(s"\t${sqa.question}\t")
          _ <- append(sqa.answers.map(a => TextRendering.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
        } yield ()
        printer.run.exec(Nil).reverse.mkString
      }

      FileManager.saveDataFile(
        experimentName,
        "wh-relationQs.tsv",
        relationTSV(whRelationWordAnalysis.sampleQuestions(new util.Random(7654326L), 1000)))
      FileManager.saveDataFile(
        experimentName,
        "nonWh-relationQs.tsv",
        relationTSV(nonWhRelationWordAnalysis.sampleQuestions(new util.Random(35265419L), 1000)))

      FileManager.saveDataFile(
        experimentName,
        "disagreementQs.tsv",
        generalTSV(sampleQAs(new util.Random(356127158L), 1.0, DisagreementAnalysis.disagreementQAs.sentenceToQAs, 1000)))

      FileManager.saveDataFile(
        experimentName,
        "random-validQAs.tsv",
        generalTSV(sampleQAs(new util.Random(23536289L), 0.2, data.sentenceToQAs, 1000)))
    }

    def makeTSV: String = {
      import scalaz._
      import Scalaz._
      import scala.language.higherKinds
      val sb = new StringBuilder
      for((id, sqas) <- data.sentenceToQAs.iterator) {
        val sentence = getTokensForId(id)
        sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
        for(sqa <- sqas.iterator) {
          val answerStrings = sqa.answers.map(TextRendering.renderSpan(sentence, _))
          val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
          val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
          sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
          sb.append(s"${sqa.id.workerId}\t")
          sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
          sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
          sb.append(answerStrings.mkString("\t") + "\t")
          sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
          sb.append("\n")
        }
      }
      sb.toString
    }

    def squadFormattedFileForWiki(excludedTitles: Set[String]): String = {
      // (validQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]])
      data.sentenceToQAs
      import argonaut._
      import Argonaut._
      val idsByFile = data.sentenceToQAs.keys.collect {
        case id @ WikiSentenceId(wikiPath) => id
      }.groupBy(_.path.filePath).filter { case (filePath, _) =>
          val title = FileManager.getWiki1kFile(filePath).get.title
          if(!excludedTitles.contains(title)) {
            true
          } else {
            System.out.println(s"Excluding file with title: $title")
            false
          }
      }

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

      def getQAJson(sentenceId: WikiSentenceId, sentenceTokens: Vector[String], qIndex: Int, question: String, answers: List[Set[Int]]) = {
        Json.obj(
          "answers" -> Json.array(answers.map(a => getAnswerSpanJson(sentenceTokens, a)): _*),
          "question" -> jString(question),
          "id" -> jString(s"${sentenceId.readableFileString}::${sentenceId.readableSentenceIndex}::$qIndex")
        )
      }

      def getSentenceJson(sentenceId: WikiSentenceId) = {
        val sentenceTokens = getTokensForId(sentenceId)
        val qas = data.sentenceToQAs(sentenceId).zipWithIndex.map {
          case (sqa, qIndex) => getQAJson(sentenceId, sentenceTokens, qIndex, sqa.question, sqa.answers)
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

    def writeAllSquadFormatted(filename: String, excludedTitles: Set[String]) = {
      val allIds = allGenInfos.map(_.hit.prompt.id).collect {
        case id @ WikiSentenceId(_) => id
      }.toSet.toList
      System.out.println(s"Writing squad file $filename")
      FileManager.saveDataFile(experimentName, filename, squadFormattedFileForWiki(excludedTitles))
    }

    lazy val sortedCountedQuestionPrefixes = {
      val prefixCounts = Scorer[List[String], Int](
        data.all.iterator
          .map(_.questionTokens)
          .map(_.map(_.toLowerCase))
          .flatMap(_.inits.toList.init)
      )
      prefixCounts.iterator.toVector.sortBy(-_._2)
    }

    def printQuestionPrefixes(n: Int) = {
      val sb = new StringBuilder
      sb.append("Parent Category Label,Product Category,Level,Sales,Path\n")
      sortedCountedQuestionPrefixes.take(n).foreach { case (tokens, count) =>
        sb.append(List(tokens.last, tokens.mkString(" > "), tokens.size, count, 1).mkString(",") + "\n")
      }
      sortedCountedQuestionPrefixes.take(n).foreach { case (tokens, count) =>
        sb.append(List(tokens.last, tokens.mkString(" > "), tokens.size, count, 203).mkString(",") + "\n")
      }

      FileManager.saveDataFile(experimentName, "q-prefixes.tsv", sb.toString)
    }
  }

  lazy val allData = new QAData(allQAs)
  lazy val ptbData = allData.filterBySentence(_.isPTB)

  lazy val allAnalysis = new CompleteAnalysis(allData)

  lazy val ptbAnalysis = new CompleteAnalysis(ptbData)

  lazy val allWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(_.isWiki))

  lazy val trainWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isTrain(id)))
  lazy val devWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isDev(id)))
  lazy val testWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isTest(id)))

  lazy val trainDevWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && (isTrain(id) || isDev(id))))
  lazy val trainDevWikiAndAllPTBAnalysis = new CompleteAnalysis(allData.filterBySentence(id => !(id.isWiki && isTest(id))))

  // scatter : length / num QA pairs

  object CoverageStats {

    // def orderedPairsCovered(sentence: Vector[String], question: String, answerIndices: Set[Int]) = {
    //   val pairs = for {
    //     qi <- getWordsInQuestion(sentence, question)
    //     ai <- answerIndices
    //   } yield (math.min(qi, ai), math.max(qi, ai))
    //   pairs.toSet
    // }

    // case class PairCoverage(
    //   id: SentenceId,
    //   pairs: Set[(Int, Int)]) {
    //   def sentence = getTokensForId(id)
    //   def numCovered = pairs.size
    //   def numPossible = math.pow(sentence.size, 2).toInt
    // }
    // def nSamplePairCoverage(n: Int): Map[SentenceId, PairCoverage] = alignedInfos.map {
    //   case (id, promptToAlignments) =>
    //     val sentence = getTokensForId(id)
    //     val wqas = sampleQAPairs(id, n)
    //     val pairs = wqas.map {
    //       case WordedQAPair(_, question, answer) => orderedPairsCovered(sentence, question, answer)
    //     }.foldLeft(Set.empty[(Int, Int)])(_ union _)
    //     id -> PairCoverage(id, pairs)
    // }.toMap

    // lazy val coverageCountsBySentence = {
    //   val coverages = (1 to 5).map(nSamplePairCoverage).toList
    //   allIds.map { id =>
    //     id -> coverages.map(_(id)).map(_.numCovered)
    //   }.toMap
    // }

    // def avgCoveragePercentages(n: Int = 20) = {
    //   coverageCountsBySentence.values.toVector
    //     .sortBy(_.last)
    //     .take(n)
    //     .map(counts => counts.map(_ * 100.0 / counts.last))
    //     .toList.transpose.map(_.mean)
    // }

    // lazy val avgQAsPerKeywordByWorker = allGenInfos.flatMap(_.assignments).groupBy(_.workerId).map {
    //   case (worker, as) => worker -> {
    //     val nums = as.flatMap(_.response).groupBy(_.wordIndex).map(_._2.size).toList
    //     (nums.mean, nums.size)
    //   }
    // }

  }

  lazy val actuallyEverythingReport = {
    val sep = "\n" + ((("=" * 40) + "\n") * 2) + "\n"
    sep + "All unfiltered data aggregate stats:\n" + allAnalysis.ValidationStats.report + sep +
      sep + "PTB unfiltered data aggregate stats:\n" + ptbAnalysis.ValidationStats.report + sep +
      sep + "Wiki unfiltered data aggregate stats:\n" + allWikiAnalysis.ValidationStats.report + sep +
      sep + "Train Wiki data:\n" + trainWikiAnalysis.ValidationStats.report + sep +
      sep + "Dev Wiki data:\n" + devWikiAnalysis.ValidationStats.report + sep +
      sep + "Test Wiki data:\n" + testWikiAnalysis.ValidationStats.report + sep +
      sep + "Penn Treebank specific analysis:\n" + PTBAnalysis.fullReport + sep +
      sep + "Penn Treebank general analysis:\n" + ptbAnalysis.report + sep +
      sep + "Train + Dev Wiki analysis:\n" + trainDevWikiAnalysis.report + sep
  }

  def printEverythingReport = FileManager.saveDataFile(experimentName, "all-reports.txt", actuallyEverythingReport)

  def printSampledQAs = trainDevWikiAnalysis.printSentencesToAnnotate

  def printSampledQAsAllQAsForSentences = trainDevWikiAnalysis.printSentencesToAnnotateNICE

  def printQGData = {
    FileManager.saveDataFile(experimentName, "train.tsv", trainWikiAnalysis.makeTSV)
    FileManager.saveDataFile(experimentName, "dev.tsv", devWikiAnalysis.makeTSV)
    FileManager.saveDataFile(experimentName, "test.tsv", testWikiAnalysis.makeTSV)
  }

  def printQAData = {
    trainWikiAnalysis.writeAllSquadFormatted(
      "squad-train.json",
      Set("Nikola Tesla", "Oxygen", "Geology", "Genghis Khan", "Imperialism"))
    devWikiAnalysis.writeAllSquadFormatted(
      "squad-dev.json",
      Set("Brain", "Emotion"))
    testWikiAnalysis.writeAllSquadFormatted(
      "squad-test.json",
      Set("Architecture", "Middle Ages", "Avicenna", "Capacitor", "Martin Luther", "Steam engine"))
  }

  // TODO get this information shit printed as fuck
  // def printWorkerStats = {
  //   // generation
  //   val genSB = new StringBuilder
  //   sb.append("assignmentId\thitId\tworkerId")
  //   for(HITInfo(hit, assignments) <- allGenInfos) {
  //     val sentence = getTokensForId(hit.prompt.id)
  //     for(assignment <- assignments) {

  //     }
  //     sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
  //     for(sqa <- sqas.iterator) {
  //       val answerStrings = sqa.answers.map(TextRendering.renderSpan(sentence, _))
  //       val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
  //       val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
  //       sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
  //       sb.append(s"${sqa.id.workerId}\t")
  //       sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
  //       sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
  //       sb.append(answerStrings.mkString("\t") + "\t")
  //       sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
  //       sb.append("\n")
  //     }
  //   }
  //   sb.toString
  // }
}
