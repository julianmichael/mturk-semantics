package mts.experiments.expH

import akka.actor.ActorRef

import java.nio.file.Paths

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.conll._
import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._

import mts.util._
import mts.core._
import mts.tasks._


trait PackagePlatformExtensions {
  case object Pring

  def getTokensForId(id: SentenceId): Vector[String] = id match {
    case PTBSentenceId(path) => getPTBSentenceTokens(
      FileManager.getPTBSentence(path).get
    )
    case WikiSentenceId(path) =>
      FileManager.getWiki1kSentence(path).get.tokens
  }

  def getWordsInQuestion(sentence: Vector[String], string: String)(implicit inflections: Inflections): Set[Int] = {
    val tokens = tokenize(string).filterNot(isReallyUninteresting)
    val moreTokens = tokens.map(t => TextRendering.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.zipWithIndex.filter(p => generalizedTokens.contains(p._1.lowerCase)).map(_._2).toSet
  }

  def getAlignedQuestionIndices(sentence: Vector[String], questionTokens: Vector[String])(implicit inflections: Inflections): Set[Int] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val allIndices = for {
      (t, index) <- questionTokens.zipWithIndex
      if !isReallyUninteresting(t)
      lowerToken = TextRendering.normalizeToken(t).lowerCase
      tokenForm <- t.lowerCase :: inflections.getAllForms(lowerToken).toList
      if lowerSentence.contains(tokenForm)
    } yield index
    allIndices.toSet
  }

  def getQuestionSentenceAlignments(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections): Set[(Int, Int)] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val lowerQuestion = questionTokens.map(_.lowerCase)
    val allIndices = for {
      (qToken, qIndex) <- lowerQuestion.zipWithIndex
      if !isReallyUninteresting(qToken)
      lowerQToken = TextRendering.normalizeToken(qToken).lowerCase
      qTokenForm <- qToken :: inflections.getAllForms(lowerQToken).toList
      (sToken, sIndex) <- lowerSentence.zipWithIndex
      lowerSToken = TextRendering.normalizeToken(sToken).lowerCase
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

  import PTBFileManager._

  def qaSRLPTBSentenceTokens = FileManager.loadResource(Paths.get("qasrl_train_sents_c09.txt"))
    .map(_.toList).tried.get
    .map(_.split(" "))

  def findQASRLPTBPaths = {
    import scala.collection.mutable
    val paths = mutable.Set.empty[PTBSentencePath]
    val sentencesNoSpaces = mutable.Set.empty[String]
    qaSRLPTBSentenceTokens
      .map(_.map(TextRendering.normalizeToken).mkString("").replaceAll("\\s", ""))
      .foreach(s => sentencesNoSpaces += s)

    allPTBSentencePaths.foreach { sPath =>
      val sentence = TextRendering.renderSentence(getPTBSentence(sPath).get).replaceAll("\\s", "")
      if(sentencesNoSpaces.contains(sentence)) {
        sentencesNoSpaces -= sentence
        paths += sPath
        println(sPath)
      }
    }

    (paths.toSet, sentencesNoSpaces.toSet)
  }

  def makeStats(
    status: SentenceStatus,
    genHITTypeId: String,
    valHITTypeId: String)(
    implicit config: TaskConfig,
    inflections: Inflections): SentenceStats = {
    val allValidations = status.finishedAssignments
    val id = status.id
    val sentence = getTokensForId(id)
    val allValHITIds = allValidations.map(_.hitId).toSet
    val valHITInfos = allValHITIds.toList
      .map(hitId => FileManager.getHITInfo[ValidationPrompt, List[ValidationAnswer]](valHITTypeId, hitId).get)
    val allGenHITIds = valHITInfos.map(_.hit.prompt.sourceHITId).toSet
    val genHITInfos = allGenHITIds.toList
      .map(hitId => FileManager.getHITInfo[GenerationPrompt, List[WordedQAPair]](genHITTypeId, hitId).get)
    val sentenceHITInfo = SentenceHITInfo(sentence, genHITInfos, valHITInfos)

    val earliestTime = util.Try(
      genHITInfos.flatMap(_.assignments).map(_.acceptTime).min
    ).toOption.getOrElse(0L)
    val latestTime = util.Try(
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
    val completionTime = util.Try(
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
    SentenceStatus(id, allKeywords, Set.empty[Int], Set.empty[ValidationPrompt], List.empty[Assignment[List[ValidationAnswer]]])
  }
}
