package mts.experiments.expB

import mts.core.Annotation
import mts.util._
import mts.util.LowerCaseStrings._
import mts.conll._
import mts.experiments.expA
import mts.experiments.expA.OpenFormExperiment.{answerValidator}
import mts.language.tokenize

case class ValidationInfo(
  val path: CoNLLSentencePath,
  val qaInfo: expA.QAInfo,
  val validationAnswer: ValidationAnswer,
  val annotation: Annotation) {

  val questionIsValid = qaInfo.questionIsValid
  val questionWasJudgedValid = validationAnswer.isValid
  val answerIsValid = validationAnswer match {
    case InvalidQuestion(_) => !questionIsValid
    case Answer(a, _) => answerValidator.isValid(qaInfo.question, tokenize(a).map(_.lowerCase))
  }
}

object ValidationInfo {
  def fromAnnotations(annotations: List[Annotation]): List[ValidationInfo] = {
    val openFormAnnotations = expA.OpenFormExperiment.getAllAnnotations
    val openFormQAInfos = expA.QAInfo.fromAnnotations(openFormAnnotations)
    val openFormQAInfosBySentence = openFormQAInfos.groupBy(_.sentence)
    for {
      anno <- annotations
      question <- anno.question.toList
      (ValidationPrompt(path, qas), ValidationResponse(answers, _)) = (ValidationExperiment.taskSpec.extractPrompt(question),
                                                                       ValidationExperiment.taskSpec.extractResponse(anno))
      (ValidationQuestion(_, origWorkerId, origQ, origA), vAnswer) <- qas.zip(answers)
      sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
      qaInfo = openFormQAInfosBySentence(sentence).find(info => info.annotation.workerId == origWorkerId &&
                                                          info.origQuestion.equals(origQ) &&
                                                          info.origAnswer.equals(origA)).get
    } yield ValidationInfo(path, qaInfo, vAnswer, anno)
  }
}
