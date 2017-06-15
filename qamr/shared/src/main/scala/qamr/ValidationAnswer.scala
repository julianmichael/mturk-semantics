package qamr

sealed trait ValidationAnswer {
  def isInvalid = this match {
    case InvalidQuestion => true
    case _ => false
  }

  def getRedundant = this match {
    case r @ Redundant(_) => Some(r)
    case _ => None
  }
  def isRedundant = !getRedundant.isEmpty

  def getAnswer = this match {
    case a @ Answer(_) => Some(a)
    case _ => None
  }
  def isAnswer = !getAnswer.isEmpty

  def isComplete = this match {
    case InvalidQuestion => true
    case Redundant(_) => true
    case Answer(indices) => !indices.isEmpty
  }
}
case object InvalidQuestion extends ValidationAnswer
case class Redundant(other: Int) extends ValidationAnswer
case class Answer(indices: Set[Int]) extends ValidationAnswer

object ValidationAnswer {
  def resolveRedundancy(va: ValidationAnswer, answers: List[ValidationAnswer]) =
    va.getRedundant.fold(va)(r => answers(r.other))

  def numAgreed(
    one: List[ValidationAnswer],
    two: List[ValidationAnswer]
  ) = {
    one.map(resolveRedundancy(_, one)).zip(
      two.map(resolveRedundancy(_, two))).filter {
      case (InvalidQuestion, InvalidQuestion) => true
      case (Answer(span1), Answer(span2)) => !span1.intersect(span2).isEmpty
      case _ => false
    }.size
  }
}
