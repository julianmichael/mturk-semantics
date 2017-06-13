package mts

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

trait PackagePlatformExtensions {

  def getWordsInQuestion(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[Int] = {
    val tokens = questionTokens.filterNot(isStopword)
    val moreTokens = tokens.map(t => Text.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.zipWithIndex.filter(p => generalizedTokens.contains(p._1.lowerCase)).map(_._2).toSet
  }

  def getAlignedQuestionIndices(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[Int] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val allIndices = for {
      (t, index) <- questionTokens.zipWithIndex
      if !isStopword(t)
      lowerToken = Text.normalizeToken(t).lowerCase
      tokenForm <- t.lowerCase :: inflections.getAllForms(lowerToken).toList
      if lowerSentence.contains(tokenForm)
    } yield index
    allIndices.toSet
  }

  def getQuestionSentenceAlignments(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[(Int, Int)] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val lowerQuestion = questionTokens.map(_.lowerCase)
    val allIndices = for {
      (qToken, qIndex) <- lowerQuestion.zipWithIndex
      if !isStopword(qToken)
      lowerQToken = Text.normalizeToken(qToken).lowerCase
      qTokenForm <- qToken :: inflections.getAllForms(lowerQToken).toList
      (sToken, sIndex) <- lowerSentence.zipWithIndex
      lowerSToken = Text.normalizeToken(sToken).lowerCase
      sTokenForm <- sToken :: inflections.getAllForms(lowerSToken).toList
      if qTokenForm.equals(sTokenForm)
    } yield (qIndex, sIndex)
    allIndices.toSet
  }

}
