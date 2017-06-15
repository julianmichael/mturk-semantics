package qamr

case class SourcedQA[SID](
  id: QAPairId[SID],
  wqa: WordedQAPair,
  validatorResponses: List[(String, ValidationAnswer)] // pairs with validator worker ID
) {
  def validatorAnswers: List[ValidationAnswer] = validatorResponses.map(_._2)
  def goodValAnswers = validatorAnswers.flatMap(_.getAnswer.map(_.indices))
  def isValid = validatorAnswers.forall(_.isAnswer)
  // def isGood = isValid && (questionWords -- Set("much", "many")).contains(questionTokens.head.toLowerCase)

  def question = wqa.question
  def answers = wqa.answer :: goodValAnswers

  // val questionTokens = tokenize(wqa.question)
  // val questionTaggedTokens = posTag(questionTokens)
}
