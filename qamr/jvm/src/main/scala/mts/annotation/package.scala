package mts.annotation

import mts._
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

import upickle.default._

trait PackagePlatformExtensions {
  case object Pring

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

  def tokenSplits(
    tokens: Vector[String])(
    implicit isStopword: IsStopword
  ) = splitList(tokens.indices.filter(i => !isStopword(tokens(i))).toList)

  case class ValidationResult[SID](
    prompt: GenerationPrompt[SID],
    sourceHITId: String,
    sourceAssignmentId: String,
    numValid: Int)

  case object SaveData

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

  def makeStats[SID : HasTokens : Reader](
    status: SentenceStatus[SID],
    genHITTypeId: String,
    valHITTypeId: String)(
    implicit config: TaskConfig,
    inflections: Inflections
  ): SentenceStats[SID] = {
    val allValidations = status.finishedAssignments
    val id = status.id
    val sentence = id.tokens
    val allValHITIds = allValidations.map(_.hitId).toSet
    val valHITInfos = allValHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[ValidationPrompt[SID], List[ValidationAnswer]](valHITTypeId, hitId).get)
    val allGenHITIds = valHITInfos.map(_.hit.prompt.sourceHITId).toSet
    val genHITInfos = allGenHITIds.toList
      .map(hitId => config.hitDataService.getHITInfo[GenerationPrompt[SID], List[WordedQAPair]](genHITTypeId, hitId).get)
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
      val keywordForms = inflections.getAllForms(sentence(keywordIndex).lowerCase)
      val qaPairs = for {
        HITInfo(hit, assignments) <- genHITInfos
        assignment <- assignments
        wqa @ WordedQAPair(_, question, answerIndices) <- assignment.response
        if (answerIndices contains keywordIndex) || (keywordForms.exists(question.lowerCase.contains))
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

  def emptyStatus[SID : HasTokens](id: SID)(implicit isStopword: IsStopword) = {
    val sentence = id.tokens
    val allKeywords = sentence.indices
      .filter(i => !isStopword(sentence(i)))
      .toSet
    SentenceStatus(
      id, allKeywords,
      Set.empty[Int], Set.empty[ValidationPrompt[SID]],
      List.empty[Assignment[List[ValidationAnswer]]])
  }
}
