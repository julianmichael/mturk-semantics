package mts

case class Question(
  xml: String,
  annotation: String
) {
  def getQuestionId = xml ++ annotation
}
