package mts

case class Annotation(
  hitType: String,
  hitId: String,
  question: Question,
  workerId: String,
  answer: String,
  submitTime: Long,
  assignmentId: String
)

