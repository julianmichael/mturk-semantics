package mts.core

case class Annotation(
  assignmentId: String,
  hitType: String,
  hitId: String,
  question: Option[Question],
  workerId: String,
  answer: String,
  acceptTime: Long,
  submitTime: Long
)

