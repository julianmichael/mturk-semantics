package mts.analysis

import mts.conll._

case class QAInfo(
  val sentence: CoNLLSentence,
  val question: List[String],
  val answer: List[String]
) {
  val sentenceTokens = sentence.words.map(_.token)
  val indexedSentence = sentenceTokens.map(_.toLowerCase).zipWithIndex

  val sentenceSet = sentenceTokens.map(_.toLowerCase).toSet
  val questionSet = question.toSet
  val answerSet = answer.toSet

  val questionOverlap = indexedSentence.filter(p => questionSet(p._1)).map(_._2).toSet
  val proportionQuestionOverlap = questionOverlap.size.toDouble / sentenceTokens.size

  val answerOverlap = indexedSentence.filter(p => answerSet(p._1)).map(_._2).toSet
  val proportionAnswerOverlap = answerOverlap.size.toDouble / sentenceTokens.size

  val newQuestionWords = question.filterNot(sentenceSet)
  val newAnswerWords = answer.filterNot(sentenceSet)

  val questionFirstWord = question.head
  val questionFirstWordIfNew = if(!sentenceSet(questionFirstWord)) Some(question.head) else None
  // bigram if both new, unigram if only first new, None if first isn't new
  val questionFirstPhrase = if(!sentenceSet.contains(questionFirstWord)) {
    None
  } else {
    question.tail.headOption.filterNot(sentenceSet) match {
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
