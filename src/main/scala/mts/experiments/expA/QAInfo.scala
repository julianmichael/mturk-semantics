package mts.experiments.expA

import mts.conll._
import mts.language

case class QAInfo(
  val sentence: CoNLLSentence,
  val question: List[String],
  val answer: List[String]
) {
  val sentenceTokens = sentence.words.map(_.token.toLowerCase)
  val indexedSentence = sentenceTokens.map(_.toLowerCase).zipWithIndex

  val tokenSets = sentenceTokens.map(OpenFormExperiment.inflections.getAllForms)
  val indexedTokenSets = tokenSets.zipWithIndex
  val allAdmissibleTokens = tokenSets.flatten.toSet

  val sentenceSet = sentenceTokens.map(_.toLowerCase).toSet
  val questionSet = question.map(_.toLowerCase).toSet
  val answerSet = answer.map(_.toLowerCase).toSet

  val questionOverlap = indexedTokenSets.filter(p => questionSet.exists(p._1.contains)).map(_._2).toSet
  val proportionQuestionOverlap = questionOverlap.size.toDouble / sentenceTokens.size

  val answerOverlap = indexedTokenSets.filter(p => answerSet.exists(p._1.contains)).map(_._2).toSet
  val proportionAnswerOverlap = answerOverlap.size.toDouble / sentenceTokens.size

  val newQuestionWords = question.filterNot(allAdmissibleTokens.contains)
  val newAnswerWords = answer.filterNot(allAdmissibleTokens.contains)

  val questionFirstWord = question.head
  val questionFirstWordIfNew = if(!allAdmissibleTokens.contains(questionFirstWord)) Some(question.head) else None
  // bigram if both new, unigram if only first new, None if first isn't new
  val questionFirstPhrase = if(!allAdmissibleTokens.contains(questionFirstWord)) {
    None
  } else {
    question.tail.headOption.filterNot(allAdmissibleTokens.contains) match {
      case None => Some(questionFirstWord)
      case Some(questionSecondWord) => Some(s"$questionFirstWord $questionSecondWord")
    }
  }

  val arcs = for {
    qIndex <- questionOverlap
    aIndex <- answerOverlap
    if qIndex != aIndex
  } yield (qIndex, aIndex)
}
