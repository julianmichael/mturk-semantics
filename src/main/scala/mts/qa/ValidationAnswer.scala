package mts.qa

sealed trait ValidationAnswer
case object InvalidQuestion extends ValidationAnswer
case class Answer(s: String) extends ValidationAnswer
