package mts.experiments.expB

import mts.core.Annotation
import mts.util._
import mts.conll._
import mts.experiments.expA
import mts.experiments.expA.OpenFormExperiment.{questionValidator, answerValidator}

case class AggregatedValidationInfo(
  val tagNames: List[String],
  val tags: List[String],
  val infos: List[ValidationInfo]
) {

  val vAnswers = infos.map(_.validationAnswer)

  // these are ONLY meaningful when we aggregate by question!
  val maxExactAgreement = vAnswers.map(vAnswer => vAnswers.filter(vAnswer.agreesExactlyWith).size).max
  val maxLooseAgreement = vAnswers.map(vAnswer => vAnswers.filter(vAnswer.overlapsWith).size).max
  val numInvalidAnswers = vAnswers.filterNot(_.isValid).size

  // validity stats: true/positive means INVALID
  val tp = infos.filter(i => !i.qaInfo.qaPairIsValid && !i.questionWasJudgedValid).size
  val fp = infos.filter(i => i.qaInfo.qaPairIsValid && !i.questionWasJudgedValid).size
  val tn = infos.filter(i => i.qaInfo.qaPairIsValid && i.questionWasJudgedValid).size
  val fn = infos.filter(i => !i.qaInfo.qaPairIsValid && i.questionWasJudgedValid).size
  val recall = tp.toDouble / (tp + fn)
  val precision = tp.toDouble / (tp + fp)
  val f1 = 2.0 * (precision * recall) / (precision + recall)
  val accuracy = (tp.toDouble + tn) / infos.size

}

object AggregatedValidationInfo {
  // could be assignment ID, HIT ID, HIT type ID...
  def aggregateBy(
    factorNames: List[String],
    extractFactors: (ValidationInfo => List[String]),
    infos: Iterable[ValidationInfo]
  ): List[AggregatedValidationInfo] = {
    infos.groupBy(extractFactors).map {
      case (factors, infos) => AggregatedValidationInfo(factorNames, factors, infos.toList)
    }.toList
  }

  def makeTSV(aggInfos: List[AggregatedValidationInfo]) = {
    val factorHeaders = aggInfos.head.tagNames.mkString("\t")
    val header = s"$factorHeaders\tmaxExactAgreement\tmaxLooseAgreement\tnumInvalidAnswers\t" +
      "precision\trecall\tf1\taccuracy\n"
    header + aggInfos.map { case aggInfo@AggregatedValidationInfo(_, factors, infos) =>
      import aggInfo._
      val factorValues = factors.mkString("\t")
      s"$factorValues\t$maxExactAgreement\t$maxLooseAgreement\t$numInvalidAnswers" +
        s"$precision\t$recall\t$f1\t$accuracy\n"
    }.mkString("\n")
  }
}
