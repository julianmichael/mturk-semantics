package mts.experiments.expA

import mts.core._
import mts.conll._
import mts.language.tokenize
import mts.validation._
import mts.util._
import mts.util.LowerCaseStrings._

case class QAInfo(
  val sentence: CoNLLSentence,
  val origQuestion: String,
  val origAnswer: String,
  val annotation: Annotation,
  val assignmentNum: Int // which assignment is it for this worker
) {
  val question: List[LowerCaseString] = tokenize(origQuestion).map(_.lowerCase)
  val answer: List[LowerCaseString] = tokenize(origAnswer).map(_.lowerCase)
  val sentenceTokens: List[LowerCaseString] = sentence.words.map(_.token.lowerCase)
  val indexedSentence = sentenceTokens.zipWithIndex

  val tokenSets = sentenceTokens.map(OpenFormExperiment.inflections.getAllForms)
  val indexedTokenSets = tokenSets.zipWithIndex
  val allAdmissibleTokens = tokenSets.flatten.toSet

  val sentenceSet = sentenceTokens.toSet
  val questionSet = question.toSet
  val answerSet = answer.toSet

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
      case Some(questionSecondWord) => Some(s"$questionFirstWord $questionSecondWord".lowerCase)
    }
  }

  val arcs = for {
    qIndex <- questionOverlap
    aIndex <- answerOverlap
    if qIndex != aIndex
  } yield (qIndex, aIndex)

  val questionIsValid = OpenFormExperiment.questionValidator.isValid(sentenceTokens, question)
  val answerIsValid = OpenFormExperiment.answerValidator.isValid(sentenceTokens, answer)
  val qaPairIsValid = questionIsValid && answerIsValid
}

object QAInfo {
  def fromAnnotations(annotations: List[Annotation]): List[QAInfo] = {
    val annosByWorker = annotations.groupBy(a => (a.workerId, a.hitType))
    for {
      (_, workerAnnos) <- annosByWorker.toList
      sortedWorkerAnnos = workerAnnos.sortBy(_.acceptTime)
      (anno, assignmentNum) <- sortedWorkerAnnos.zipWithIndex
      question <- anno.question.toList
      ((path, _), (qaPairs, _)) = (OpenFormExperiment.protoTaskSpec.extractQuestionData(question),
                                       OpenFormExperiment.protoTaskSpec.extractAnswerData(anno))
      sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
      (q, a) <- qaPairs
    } yield QAInfo(sentence, q, a, anno, assignmentNum)
  }
}
