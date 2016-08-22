package mts.core

case class Assignment[Response](
  hitType: String,
  hitId: String,
  assignmentId: String,
  workerId: String,
  acceptTime: Long,
  submitTime: Long,
  response: Response,
  feedback: String)

