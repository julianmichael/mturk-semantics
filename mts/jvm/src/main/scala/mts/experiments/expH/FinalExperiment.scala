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
  val experimentName = finalExperimentName

  val approvalRateRequirement = new QualificationRequirement(
    RequesterService.APPROVAL_RATE_QUALIFICATION_TYPE_ID,
    Comparator.GreaterThanOrEqualTo, 95,
    null, true)

  // saved these manually, see code in package.scala
  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    FileManager.loadDataFile(experimentName, "origQASRLPaths.txt").get.head
  )

  lazy val random250PTBSentencePaths = {
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
    case GenerationApiRequest(path) =>
      GenerationApiResponse(getPTBTokens(path))
  }

  val sampleGenPrompt = GenerationPrompt(origQASRLPaths.head, List(0, 1, 2, 3))

  lazy val genTaskSpec = TaskSpecification[GenerationPrompt, List[WordedQAPair], GenerationApiRequest, GenerationApiResponse](
    TaskIndex.expHGenerationTaskKey, genHITType, genApiFlow, sampleGenPrompt,
    frozenHITTypeId = Some("3ML4SXAKAKLJDQ3U44DJTOOA2IFB92"))

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
    case ValidationApiRequest(path) =>
      ValidationApiResponse(getPTBTokens(path))
  }

  val sampleValPrompt = ValidationPrompt(
    sampleGenPrompt, "", "",
    List(WordedQAPair(0, "Who is awesome?", Set(1, 2, 3, 4)),
         WordedQAPair(1, "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val valTaskSpec = TaskSpecification[ValidationPrompt, List[ValidationAnswer], ValidationApiRequest, ValidationApiResponse](
    TaskIndex.expHValidationTaskKey, valHITType, valApiFlow, sampleValPrompt,
    frozenHITTypeId = Some("3KBOIXB475RNG937KWXSCXE72MX5QH"))

  // TODO get the right prompts

  // hit management --- circularly defined so they can communicate

  def splitNum(n: Int): List[Int] =
    if(n <= 0) Nil
    else if(n <= 3) List(n)
    else if(n == 5) List(2, 3)
    else if(n == 6) List(3, 3)
    else if(n == 9) List(3, 3, 3)
    else 4 :: splitNum(n - 4)

  def splitList(l: List[Int]) = splitNum(l.size)
    .foldLeft((l, List.empty[List[Int]])) {
    case ((remaining, groups), groupSize) =>
      (remaining.drop(groupSize), remaining.take(groupSize) :: groups)
  }._2

  def pathSplits(path: PTBSentencePath) = {
    val tokens = getPTBTokens(path)
    splitList(tokens.indices.filter(i => !isReallyUninteresting(tokens(i))).toList)
  }

  val sourcePrompts = random250PTBSentencePaths
    .take(30)
    .flatMap(path => pathSplits(path).map(GenerationPrompt(path, _)))

  import config.actorSystem

  lazy val sentenceTracker: ActorRef = actorSystem.actorOf(Props(new SentenceTracker))

  lazy val genHelper = new HITManager.Helper(genTaskSpec)
  lazy val genManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props(new GenerationHITManager(
              genHelper,
              valHelper,
              valManager,
              sentenceTracker,
              2, 20, sourcePrompts.iterator)))
  } else {
    actorSystem.actorOf(
      Props(new GenerationHITManager(
              genHelper,
              valHelper,
              valManager,
              sentenceTracker,
              1, 3, sourcePrompts.iterator)))
  }

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = if(config.isProduction) {
    actorSystem.actorOf(
      Props(ValidationHITManager(
              valHelper,
              genManager,
              sentenceTracker,
              2, 20)))
  } else {
    actorSystem.actorOf(
      Props(ValidationHITManager(
              valHelper,
              genManager,
              sentenceTracker,
              1, 3)))
  }

  lazy val server = new Server(List(genTaskSpec, valTaskSpec))
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

  def renderValidationAnswer(sentence: PTBSentence, va: ValidationAnswer, referenceQAs: List[WordedQAPair]): String = va match {
    case InvalidQuestion => "<Invalid>"
    case Redundant(i) => s"<Redundant with ${referenceQAs(i).question}>"
    case Answer(span) => TextRendering.renderSpan(sentence, span)
  }

  def renderValidation(info: HITInfo[ValidationPrompt, List[ValidationAnswer]]) = {
    val sentence = FileManager.getPTBSentence(info.hit.prompt.genPrompt.path).get
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

  lazy val makeTSV: String = {
    val sb = new StringBuilder
    val paths = sourcePrompts.map(_.path).toSet.toList
    val genInfos = allGenInfos; val valInfos = allValInfos
    import scalaz._
    import Scalaz._
    import scala.language.higherKinds
    type PrintingState[A] = State[List[String], A]
    type Printing[A] = ListT[PrintingState, A]
    def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
    def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))
    val processor = for {
      path <- iter(paths)
      sentence = FileManager.getPTBSentence(path).get
      sentenceTokens = TextRendering.getTokens(sentence)
      _ <- append("\t\t\t" + TextRendering.renderSentence(sentence) + "\n")
      (genWorkerId, WordedQAPair(keywordIndex, question, answerIndices), valAnswers, valAnswersString) <- iter {
        val qaPairs = for {
          HITInfo(genHIT, genAssignments) <- genInfos
          if genHIT.prompt.path == path
          genAssignment <- genAssignments
          chosenValInfo = valInfos.find(_.hit.prompt.sourceAssignmentId.equals(genAssignment.assignmentId)).get
          (wqa, qaIndex) <- genAssignment.response.zipWithIndex
          valAnswers = chosenValInfo.assignments.map(a => a.response(qaIndex))
        } yield (
          genAssignment.workerId,
          wqa,
          valAnswers,
          valAnswers.map(valAnswer =>
            renderValidationAnswer(sentence, valAnswer, genAssignment.response)
          ).mkString("\t")
          )
        qaPairs.sortBy(_._2.wordIndex)
      }
      _ <- append(s"${path.filePath.suffix}\t${path.sentenceNum}\t${genWorkerId}\t")
      _ <- append(s"${sentenceTokens(keywordIndex)} ($keywordIndex)\t$question\t")
      _ <- append(TextRendering.renderSpan(sentence, answerIndices) + s"\t$valAnswersString\t")
      _ <- append(s"${answerIndices.mkString(" ")}\t")
      _ <- append(valAnswers.map(_.getAnswer.map(_.indices.mkString(" ")).getOrElse("")).mkString("\t"))
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
}
