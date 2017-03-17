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
import com.amazonaws.mturk.service.axis.RequesterService
import com.amazonaws.mturk.requester.Comparator

class FinalExperiment(implicit config: TaskConfig) {
  val experimentName = finalExperimentName

  val approvalRateRequirement = new QualificationRequirement(
    RequesterService.APPROVAL_RATE_QUALIFICATION_TYPE_ID,
    Comparator.GreaterThanOrEqualTo, 95,
    null, true)

  // saved these manually, see code in package.scala
  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    FileManager.loadDataFile(experimentName, "origQASRLPaths.txt").get.head
  )


  val numPTB = 10 // up to 250

  lazy val (ptbTrain, ptbDev, ptbTest) = {
    val shuffleRand = new util.Random(987654321L)
    val (train, devTestRest) = shuffleRand.shuffle(origQASRLPaths).splitAt(numPTB * 4 / 5)
    val (dev, testRest) = devTestRest.splitAt(numPTB / 10)
    val test = testRest.take(numPTB / 10)
    (train, dev, test)
  }

  def getWikiSentences(rand: util.Random, filePaths: Vector[Wiki1kPath], numSentences: Int) = {
    rand.shuffle(
      filePaths.flatMap(p => FileManager.getWiki1kFile(p).get.paragraphs)
    ).filter(p =>
      !p.exists(sentence => TextRendering.renderSentence(sentence).contains("formula_"))
    ).flatten.map(s => s.path).take(numSentences)
  }

  val numWikipedia = 10

  lazy val (wikipediaTrain, wikipediaDev, wikipediaTest) = {
    val shuffleRand = new util.Random(329358L)
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

  val numWikinews = 10 // up to 2500

  lazy val (wikinewsTrain, wikinewsDev, wikinewsTest) = {
    val shuffleRand = new util.Random(1846178L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      FileManager.wiki1kPathsForDomain("wikinews")
        .sortBy(-_.suffix.toInt) // XXX relies on wikinews IDs being ints... make typeful
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
    title = s"Write questions and answers about words in context",
    description = s"""
      Given a sentence and a set of words from that sentence,
      write questions and answers involving each word.
      Write more question-answer pairs for increasing bonuses!
    """.trim,
    reward = generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](approvalRateRequirement))

  lazy val genApiFlow = Flow[GenerationApiRequest].map {
    case GenerationApiRequest(id) =>
      GenerationApiResponse(getTokensForId(id))
  }

  lazy val sampleGenPrompt = GenerationPrompt(PTBSentenceId(origQASRLPaths.head), List(0, 1, 2, 3))

  lazy val genTaskSpec = TaskSpecification[GenerationPrompt, List[WordedQAPair], GenerationApiRequest, GenerationApiResponse](
    TaskIndex.expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt)

  // validation task definition

  val valHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and several questions,
      highlight the part of the sentence that answers each question.
    """.trim,
    reward = validationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](approvalRateRequirement))

  lazy val valApiFlow = Flow[ValidationApiRequest].map {
    case ValidationApiRequest(id) =>
      ValidationApiResponse(getTokensForId(id))
  }

  lazy val sampleValPrompt = ValidationPrompt(
    sampleGenPrompt, "", "",
    List(WordedQAPair(0, "Who is awesome?", Set(1, 2, 3, 4)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val valTaskSpec = TaskSpecification[ValidationPrompt, List[ValidationAnswer], ValidationApiRequest, ValidationApiResponse](
    TaskIndex.expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt)

  // hit management --- circularly defined so they can communicate

  lazy val sourceIds = {
    val idShuffleRand = new util.Random(218469L)
    idShuffleRand.shuffle(trainIds ++ devIds ++ testIds)
  }

  lazy val sourcePrompts = sourceIds
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

  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props {
        genManagerPeek = new GenerationHITManager(
          genHelper,
          valHelper,
          valManager,
          sentenceTracker,
          1, 30, sourcePrompts.iterator)
        genManagerPeek
      })
  } else {
    actorSystem.actorOf(
      Props {
        genManagerPeek = new GenerationHITManager(
          genHelper,
          valHelper,
          valManager,
          sentenceTracker,
          1, 3, sourcePrompts.iterator)
        genManagerPeek
      })
  }

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props {
        valManagerPeek = ValidationHITManager(
          valHelper,
          genManager,
          sentenceTracker,
          2, 50)
        valManagerPeek
      })
  } else {
    actorSystem.actorOf(
      Props {
        valManagerPeek = ValidationHITManager(
          valHelper,
          genManager,
          sentenceTracker,
          1, 3)
        valManagerPeek
      })
  }

  val dashboardApiFlow = Flow[Unit]
    .merge(Source.tick(initialDelay = 0.seconds, interval = 1.minute, ()))
    .filter(_ => genManagerPeek != null && valManagerPeek != null && sentenceTrackerPeek != null)
    .map { _ =>
    val last5Sentences = sentenceTrackerPeek.finishedSentenceStats.take(5).map { stats =>
      val sentence = getTokensForId(stats.id)
      stats -> SentenceHITInfo(
        sentence,
        stats.genHITIds.toList
          .map(FileManager.getHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId, _))
          .map(_.get),
        stats.valHITIds.toList
          .map(FileManager.getHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId, _))
          .map(_.get))
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
    genActor ! Start(interval, delay = 0 seconds)
    valActor ! Start(interval, delay = 3 seconds)
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
  def save() = {
    sentenceTracker ! SaveData
    genManager ! SaveData
    valManager ! SaveData
  }

  // convenience functions

  def allGenInfos = FileManager.loadAllHITInfo[GenerationPrompt, List[WordedQAPair]](genTaskSpec.hitTypeId)
  def allValInfos = FileManager.loadAllHITInfo[ValidationPrompt, List[ValidationAnswer]](valTaskSpec.hitTypeId)

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

  def makeTSV: String = {
    val sb = new StringBuilder
    val ids = sourcePrompts.map(_.id).toSet.toList
    val genInfos = allGenInfos; val valInfos = allValInfos
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
      _ <- append("\t\t\t" + TextRendering.renderSentence(sentence) + "\n")
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
      _ <- append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${genWorkerId}\t")
      _ <- append(s"${sentence(keywordIndex)} ($keywordIndex)\t$question\t")
      _ <- append(TextRendering.renderSpan(sentence, answerIndices) + s"\t$valAnswersString\t")
      _ <- append(s"${answerIndices.mkString(" ")}\t")
      _ <- append(valAnswers.map(_.getAnswer.map(_.indices.mkString(" ")).getOrElse("")).mkString("\t"))
      _ <- append(s"\t$valFeedback")
      _ <- append("\n")
    } yield ()
    processor.run.exec(Nil).reverse.mkString
  }

  def writeTSV = FileManager.saveDataFile(experimentName, "readable.tsv", makeTSV)

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
}
