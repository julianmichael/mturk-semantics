package example.ai2

case class MathProblemId(id: Int)

sealed trait Ai2SentenceId

object Ai2SentenceId {
  def toString(id: Ai2SentenceId) = id match {
    case KBSentenceId(pairIndex, isKBSentence) =>
      val kbIndicator = if(isKBSentence) 0 else 1
      s"Aristo: $pairIndex $kbIndicator"
    case MathProblemSentenceId(problemId, sentenceIndex) =>
      s"Euclid: $problemId $sentenceIndex"
  }

  // TODO
  def fromString(s: String): Ai2SentenceId = {
    ???
  }
}

case class KBSentenceId(
  pairIndex: Int,
  isKBSentence: Boolean
) extends Ai2SentenceId {
  def isQASentence = !isKBSentence
}

case class MathProblemSentenceId(
  problemId: Int,
  sentenceIndex: Int
) extends Ai2SentenceId


case class MathProblem(
  answer: String,
  id: Int,
  background: Vector[Vector[String]],
  question: Vector[String],
  tags: Set[String])

case class KBSentencePair(
  kbSentence: String,
  qaSentence: String)
