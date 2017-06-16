package qamr.aristo

case class MathProblemId(id: Int)

sealed trait Ai2SentenceId
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
