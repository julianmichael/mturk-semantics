package mts.analysis

import mts.conll._
import mts.util._
import mts.language._

class InterAnnotatorAgreement(
  answerSets: List[List[Option[Set[Int]]]],
  answerSetsWithAnnotators: List[List[(String, Option[Set[Int]])]]
) {
  lazy val pluralities = answerSets.map(as => as.groupBy(identity).map(_._2.size).max)
  lazy val pluralityCounts = Scorer[Int, Int](pluralities)

  lazy val validityRate = answerSets.flatten.map(a => if(a.isEmpty) 0.0 else 1.0).mean

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

  lazy val avgAgreement = {
    val props = for {
      answers <- answerSets
      (a1, i) <- answers.zipWithIndex
      a2 <- answers.take(i) ++ answers.drop(i + 1)
    } yield (a1, a2) match {
      case (None, None) => 1.0
      case (Some(x), Some(y)) => if((x intersect y).isEmpty) 0.0 else 1.0
      case _ => 0.0
    }
    props.mean
  }

  lazy val avgAgreementByAnnotator = {
    val props = for {
      answers <- answerSetsWithAnnotators
      ((worker1, a1), i) <- answers.zipWithIndex
      (worker2, a2) <- answers.drop(i + 1)
      workers = List(worker1, worker2)
      (worker, score) <- (
        (a1, a2) match {
          case (None, None) => workers.map((_, 1.0))
          case (Some(x), Some(y)) => if((x intersect y).isEmpty) workers.map((_, 0.0)) else workers.map((_, 1.0))
          case _ => workers.map((_, 0.0))
        }
      )
    } yield (worker, score)
    val scoresByWorker = props.groupBy(_._1).map {
      case (w, scores) => w -> (scores.map(_._2).mean, scores.size)
    }
    scoresByWorker
  }

  def report: String = s"""
Inter-Annotator Agreement:
Pluralities:
${histogramString(pluralityCounts)}
Mean: ${pluralities.mean}
Mean plurality proportion: $meanPluralityProp
Validity rate: $validityRate
Expected overlap: $expectedOverlap
Average ageement: $avgAgreement
Agreement by worker:\n${avgAgreementByAnnotator.mkString("\n")}
"""
}
