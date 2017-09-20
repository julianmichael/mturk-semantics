package turksem.qasrl

import cats.implicits._

import nlpdata.util.Text
import turksem.util._

/** Represents a validator response about a question:
  * either it has an answer, is invalid, or is redundant with another question.
  */
sealed trait QASRLValidationAnswer {
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
    case Answer(indices) => indices.exists(_.nonEmpty)
  }
}
case object InvalidQuestion extends QASRLValidationAnswer
case class Redundant(other: Int) extends QASRLValidationAnswer
case class Answer(spans: List[Set[Int]]) extends QASRLValidationAnswer

object QASRLValidationAnswer {
  def resolveRedundancy(va: QASRLValidationAnswer, answers: List[QASRLValidationAnswer]) =
    va.getRedundant.fold(va)(r => answers(r.other))

  def numAgreed(
    one: List[QASRLValidationAnswer],
    two: List[QASRLValidationAnswer]
  ) = {
    one.map(resolveRedundancy(_, one)).zip(
      two.map(resolveRedundancy(_, two))).filter {
      case (InvalidQuestion, InvalidQuestion) => true
      case (Answer(spans1), Answer(spans2)) =>
        spans1.exists(span1 =>
          spans2.exists(span2 =>
            span1.intersect(span2).nonEmpty
          )
        )
      case _ => false
    }.size
  }

  def numValidQuestions(responses: List[List[QASRLValidationAnswer]]) =
    math.round(responses.map(_.filter(_.isAnswer).size).meanOpt.get - 0.01).toInt

  // render a validation answer for the purpose of writing to a file
  // (just writes the highlighted indices of the answer; not readable)
  def renderIndices(
    va: QASRLValidationAnswer
  ): String = va match {
    case InvalidQuestion => "Invalid"
    case Redundant(i) => s"Redundant: $i"
    case Answer(spans) => spans.map(_.toVector.sorted.mkString(" ")).mkString("/")
  }

  val RedundantMatch = "Redundant: ([0-9]*)".r

  // inverse of QASRLValidationAnswer.renderIndices
  def readIndices(
    s: String
  ): QASRLValidationAnswer = s match {
    case "Invalid" => InvalidQuestion
    case RedundantMatch(i) => Redundant(i.toInt)
    case other => Answer(
      other.split("/").toList.map(is =>
        is.split(" ").map(_.toInt).toSet
      )
    )
  }

  // render a validation response in a readable way for browsing
  def render(
    sentence: Vector[String],
    va: QASRLValidationAnswer,
    referenceQAs: List[VerbQA]
  ): String = va match {
    case InvalidQuestion => "<Invalid>"
    case Redundant(i) => s"<Redundant with ${referenceQAs(i).question}>"
    case Answer(spans) => spans.map(Text.renderSpan(sentence, _)).mkString(" / ")
  }
}
