package mts.analysis

import mts.conll._
import mts.util._
import mts.language._

class InterAnnotatorAgreement(
  answerSets: List[List[Option[Set[Int]]]]
) {
  lazy val pluralities = answerSets.map(as => as.groupBy(identity).map(_._2.size).max)
  lazy val pluralityCounts = Scorer[Int, Int](pluralities)

  lazy val meanPluralityProp = answerSets.map(
    as => {
      val sizes = as.groupBy(identity).map(_._2.size)
      sizes.max.toDouble / sizes.sum
    }).mean

  lazy val expectedOverlap = {
    val props = for {
      answers <- answerSets
      (a1, i) <- answers.zipWithIndex
      a2 <- answers.take(i) ++ answers.drop(i + 1)
    } yield (a1, a2) match {
      case (None, None) => 1.0
      case (Some(x), Some(y)) => (x intersect y).size.toDouble / (x union y).size
      case _ => 0.0
    }
    props.mean
  }

  def report: String = s"""
Inter-Annotator Agreement:
Pluralities:
${histogramString(pluralityCounts)}
Mean: ${pluralities.mean}
Mean plurality proportion: $meanPluralityProp
Expected overlap: $expectedOverlap
"""
}
