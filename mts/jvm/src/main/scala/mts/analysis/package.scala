package mts

import mts.util._
import mts.conll._

package object analysis {
  case class QuestionAnswerPair(question: String, answer: Set[Int])

  case class AlignedQuestionAnswerPair(
    qaPair: QuestionAnswerPair,
    questionIndices: Set[Int])

  case class AnnotatedSentence(
    path: CoNLLSentencePath,
    alignedQAPairs: List[AlignedQuestionAnswerPair]) {
    def sentence = FileManager.getCoNLLSentence(path).get
  }

  case class PrecisionRecall(
    numPredicted: Double,
    numGold: Double,
    numCorrect: Double,
    numCovered: Double
  ) {
    val precision = numCorrect / numPredicted
    val recall = numCovered / numGold
    val f1 = 2 * precision * recall / (precision + recall)

    def aggregate(other: PrecisionRecall) = PrecisionRecall(
      numPredicted + other.numPredicted,
      numGold + other.numGold,
      numCorrect + other.numCorrect,
      numCovered + other.numCovered)
  }


  def statString(pr: PrecisionRecall) =
    f"F1: ${pr.f1}%.3f\tPrecision: ${pr.precision}%.3f\tRecall: ${pr.recall}%.3f"

  def histogramString(hist: Scorer[Int, Int]): String = {
    val vec = (0 to hist.keyIterator.max).map(hist.get).toVector
    val max = vec.max
    val scaleMax = 50.0
    val scaleFactor = scaleMax / max
    def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
    def pounds(n: Int) = "#" * n
    vec.zipWithIndex
      .map { case (n, i) => f"$i%3d|${pounds(scale(n))}%s $n%d"}
      .mkString("\n")
  }

  // case class JustPrecisionRecall(
  //   precision: Double,
  //   recall: Double
  // ) {
  //   val f1 = 2 * precision * recall / (precision + recall)

  //   def average(other: JustPrecisionRecall) = JustPrecisionRecall(
  //     (precision + other.precision) / 2.0,
  //     (recall + other.recall) / 2.0)
  // }


  // case class Multiedge(
  //   sentence: CoNLLSentence,
  //   questionIndices: Set[Int],
  //   answerIndices: Set[Int],
  //   questions: List[String],
  //   mutualInformations: List[Double]) {
  //   def questionWords: String = renderSpan(sentence, questionIndices)
  //   def answerWords: String = renderSpan(sentence, answerIndices)

  //   val maxMI = mutualInformations.max
  //   val avgMI = mutualInformations.sum / mutualInformations.size

  //   def toStringPretty = {
  //     f"$questionWords%15s --> $answerWords%15s $maxMI%.2f\n\t\t$questions"
  //   }
  // }

  // def getMultiEdges(sentence: CoNLLSentence, scores: List[(Int, Int, Double, List[(String, Set[Int], Set[Int])])]) = {
  //   val processedScores = scores.map {
  //     case (qi, ai, mi, qaPairs) =>
  //       def majorities(sets: Iterable[Set[Int]]): Set[Int] = {
  //         sets.flatten.toSet
  //           .filter(ai => sets.filter(_.contains(ai)).size >= (sets.size / 2))
  //       }
  //       val questions = qaPairs.map(_._1)
  //       val protoQuestionSpan = majorities(qaPairs.map(_._2))
  //       val protoAnswerSpan = majorities(qaPairs.map(_._3))
  //       (qi, ai, mi, protoQuestionSpan, protoAnswerSpan, questions)
  //   }
  //   val multiedges = for {
  //     ((qSpan, aSpan), entries) <- processedScores.groupBy(t => (t._4, t._5)).toVector
  //     mis = entries.map(_._3).toList
  //     questions = entries.map(_._6).flatten.toSet.toList
  //   } yield Multiedge(sentence, qSpan, aSpan, questions, mis)
  //   multiedges.sortBy(-_.maxMI)
  // }
}
