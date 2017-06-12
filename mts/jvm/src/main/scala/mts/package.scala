package mts

import mts.util._

import turkey._
import turkey.tasks._

import nlpdata.datasets._
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import scala.util.Try

import resource.managed
import resource.ManagedResource

trait PackagePlatformExtensions {

  val resourcePath = java.nio.file.Paths.get("resources")

  val PTB = new ptb.PTBFileSystemService(
    resourcePath.resolve("ptb")
  )

  val PropBank = new propbank.PropBankFileSystemService(
    resourcePath.resolve("propbank")
  )

  val NomBank = new nombank.NomBankFileSystemService(
    resourcePath.resolve("nombank.1.0"), PTB
  )

  val QASRL = new qasrl.QASRLFileSystemService(
    resourcePath.resolve("qasrl"), PTB
  )

  val Wiki1k = new wiki1k.Wiki1kFileSystemService(
    resourcePath.resolve("wiki1k")
  )

  val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  import java.nio.file.{Paths, Path, Files}
  private[this] val experimentRootPath = Paths.get("experiments")
  private[this] val dataPath = Paths.get("data")

  def saveDataFile(
    experimentName: String,
    fileName: String,
    contents: String
  ) = Try {
    val directory = experimentRootPath.resolve(experimentName).resolve(dataPath)
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(fileName)
    Files.write(path, contents.getBytes())
  }

  def loadDataFile(
    experimentName: String,
    fileName: String
  ) = {
    val directory = experimentRootPath.resolve(experimentName).resolve(dataPath)
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(fileName)
    import scala.collection.JavaConverters._
    managed(Files.lines(path)).map(_.iterator.asScala.toList).tried
  }

  case object Pring

  def getTokensForId(id: SentenceId): Vector[String] = id match {
    case PTBSentenceId(path) => getPTBSentenceTokens(
      PTB.getSentence(path).get
    )
    case WikiSentenceId(path) =>
      Wiki1k.getSentence(path).get.tokens
  }

  def getWordsInQuestion(
    sentence: Vector[String],
    string: String)(
    implicit inflections: Inflections
  ): Set[Int] = {
    val tokens = tokenize(string).filterNot(isReallyUninteresting)
    val moreTokens = tokens.map(t => Text.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.zipWithIndex.filter(p => generalizedTokens.contains(p._1.lowerCase)).map(_._2).toSet
  }

  def getAlignedQuestionIndices(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections
  ): Set[Int] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val allIndices = for {
      (t, index) <- questionTokens.zipWithIndex
      if !isReallyUninteresting(t)
      lowerToken = Text.normalizeToken(t).lowerCase
      tokenForm <- t.lowerCase :: inflections.getAllForms(lowerToken).toList
      if lowerSentence.contains(tokenForm)
    } yield index
    allIndices.toSet
  }

  def getQuestionSentenceAlignments(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections
  ): Set[(Int, Int)] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val lowerQuestion = questionTokens.map(_.lowerCase)
    val allIndices = for {
      (qToken, qIndex) <- lowerQuestion.zipWithIndex
      if !isReallyUninteresting(qToken)
      lowerQToken = Text.normalizeToken(qToken).lowerCase
      qTokenForm <- qToken :: inflections.getAllForms(lowerQToken).toList
      (sToken, sIndex) <- lowerSentence.zipWithIndex
      lowerSToken = Text.normalizeToken(sToken).lowerCase
      sTokenForm <- sToken :: inflections.getAllForms(lowerSToken).toList
      if qTokenForm.equals(sTokenForm)
    } yield (qIndex, sIndex)
    allIndices.toSet
  }

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

  def idSplits(id: SentenceId) = {
    val tokens = getTokensForId(id)
    splitList(tokens.indices.filter(i => !isReallyUninteresting(tokens(i))).toList)
  }

  case class ValidationResult(
    prompt: GenerationPrompt,
    sourceHITId: String,
    sourceAssignmentId: String,
    numValid: Int)

  case object SaveData

  def makeStats(
    status: SentenceStatus,
    genHITTypeId: String,
    valHITTypeId: String)(
    implicit config: TaskConfig,
    inflections: Inflections
  ): SentenceStats = {
    val allValidations = status.finishedAssignments
    val id = status.id
    val sentence = getTokensForId(id)
    val allValHITIds = allValidations.map(_.hitId).toSet
    val valHITInfos = allValHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[ValidationPrompt, List[ValidationAnswer]](valHITTypeId, hitId).get)
    val allGenHITIds = valHITInfos.map(_.hit.prompt.sourceHITId).toSet
    val genHITInfos = allGenHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[GenerationPrompt, List[WordedQAPair]](genHITTypeId, hitId).get)
    val sentenceHITInfo = SentenceHITInfo(sentence, genHITInfos, valHITInfos)

    val earliestTime = Try(
      genHITInfos.flatMap(_.assignments).map(_.acceptTime).min
    ).toOption.getOrElse(0L)
    val latestTime = Try(
      (valHITInfos.flatMap(_.assignments).map(_.submitTime) ++
         genHITInfos.flatMap(_.assignments).map(_.submitTime)).max
    ).toOption.getOrElse(0L)

    val alignedValidations = sentenceHITInfo.alignValidations
    val allKeywords = genHITInfos.map(_.hit.prompt.keywords).flatten.toSet
    val qaPairsEachKeywordPrompt = for {
      HITInfo(hit, assignments) <- genHITInfos
      assignment <- assignments
      keywordIndex <- hit.prompt.keywords.toList
    } yield assignment.response.filter(_.wordIndex == keywordIndex).size
    val qaPairsEachKeywordActual = for {
      keywordIndex <- allKeywords.toList
    } yield {
      val qaPairs = for {
        HITInfo(hit, assignments) <- genHITInfos
        assignment <- assignments
        wqa @ WordedQAPair(_, question, answerIndices) <- assignment.response
        wordsInQuestion = getWordsInQuestion(sentence, question)
        if (wordsInQuestion union answerIndices).contains(keywordIndex)
      } yield wqa
      qaPairs.size
    }
    val validationLatencies = for {
      HITInfo(_, assignments) <- genHITInfos
      assignment <- assignments
      validations = for {
        HITInfo(valHIT, valAssignments) <- valHITInfos
        if valHIT.prompt.sourceAssignmentId == assignment.assignmentId
      } yield valAssignments.map(_.submitTime).max
      if !validations.isEmpty
      completion = validations.max
    } yield ((completion - assignment.submitTime) / 1000L).toInt  // seconds

    val numQAPairs = genHITInfos.flatMap(_.assignments).flatMap(_.response).size
    val numValidQAPairs = alignedValidations
      .map(av => numValidQuestions(av.valAssignments.map(_.response)))
      .sum
    val completionTime = Try(
      valHITInfos.flatMap(_.assignments).map(_.submitTime).max
    ).toOption.getOrElse(0L)
    val genCost = alignedValidations.map(_.genCost).sum
    val valCost = alignedValidations.map(_.valCost).sum
    val genHITIds = genHITInfos.map(_.hit.hitId).toSet
    val valHITIds = valHITInfos.map(_.hit.hitId).toSet
    SentenceStats(
      id,
      earliestTime, latestTime,
      allKeywords.size,
      numQAPairs,
      numValidQAPairs,
      qaPairsEachKeywordPrompt,
      qaPairsEachKeywordActual,
      validationLatencies,
      completionTime,
      genCost, valCost,
      genHITIds, valHITIds)
  }

  def emptyStatus(id: SentenceId) = {
    val sentence = getTokensForId(id)
    val allKeywords = sentence.indices
      .filter(i => !isReallyUninteresting(sentence(i)))
      .toSet
    SentenceStatus(
      id, allKeywords,
      Set.empty[Int], Set.empty[ValidationPrompt],
      List.empty[Assignment[List[ValidationAnswer]]])
  }

  // Analysis-related data structures

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
}
