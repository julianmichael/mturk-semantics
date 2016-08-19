package mts.experiments.expA

import mts.conll._
import mts.core._

// infos must be for the same sentence
// this can be used both for per-assignment stats and per-sentence stats
case class AggregatedQAInfo(
  val tagNames: List[String],
  val tags: List[String],
  val qas: List[QAInfo]
) {
  val sentence = qas.head.sentence

  val questionOverlap = qas.map(_.questionOverlap).reduce(_ union _)
  val questionOverlapCount = questionOverlap.size
  val questionOverlapPerQA = questionOverlapCount.toDouble / qas.size
  val questionOverlapProportion = questionOverlap.size.toDouble / sentence.words.size

  val answerOverlap = qas.map(_.answerOverlap).reduce(_ union _)
  val answerOverlapCount = answerOverlap.size
  val answerOverlapPerQA = answerOverlapCount.toDouble / qas.size
  val answerOverlapProportion  = answerOverlap.size.toDouble / sentence.words.size

  val arcs = qas.map(_.arcs).reduce(_ union _)

  val coveredArgLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    spanWord <- words
    if (arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index)) && !label.equals("V")
  } yield label
  val uncoveredArgLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    spanWord <- words
    if !(arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index)) && !label.equals("V")
  } yield label
  val coveredArgLabelCount = coveredArgLabels.size
  val uncoveredArgLabelCount = uncoveredArgLabels.size
  val coveredLabelProportion = coveredArgLabels.size.toDouble / (coveredArgLabels.size + uncoveredArgLabels.size)

  val someWordCoveredLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    if !label.equals("V") && words.exists(spanWord =>
      arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index))
  } yield label
  val noWordCoveredLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    if !label.equals("V") && !words.exists(spanWord =>
      arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index))
  } yield label
  val someWordCoveredLabelCount = someWordCoveredLabels.size
  val noWordCoveredLabelCount = noWordCoveredLabels.size
  val someWordCoveredLabelProportion = someWordCoveredLabelCount.toDouble / (someWordCoveredLabelCount + noWordCoveredLabelCount)

  val allWordsCoveredLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    if !label.equals("V") && words.forall(spanWord =>
      arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index))
  } yield label
  val someWordUncoveredLabels = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(label, words) <- args
    if !label.equals("V") && !words.forall(spanWord =>
      arcs.contains(pred.head.index, spanWord.index) || arcs.contains(spanWord.index, pred.head.index))
  } yield label
  val allWordsCoveredLabelCount = allWordsCoveredLabels.size
  val someWordUncoveredLabelCount = someWordUncoveredLabels.size
  val allWordsCoveredLabelProportion = allWordsCoveredLabelCount.toDouble / (allWordsCoveredLabelCount + someWordUncoveredLabelCount)

  val arcMatches = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(depLabel, argWords) <- args
    if !depLabel.equals("V")
    qaInfo <- qas
    questionLabel <- qaInfo.questionFirstWordIfNew.toList
    word <- argWords
    if qaInfo.arcs.contains(pred.head.index, word.index) || qaInfo.arcs.contains(word.index, pred.head.index)
  } yield (depLabel, questionLabel)

  val arcMatchesForSome = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(depLabel, argWords) <- args
    if !depLabel.equals("V")
    qaInfo <- qas
    questionLabel <- qaInfo.questionFirstWordIfNew.toList
    if argWords.exists(word =>
      qaInfo.arcs.contains(pred.head.index, word.index) || qaInfo.arcs.contains(word.index, pred.head.index))
  } yield (depLabel, questionLabel)

  val arcMatchesForAll = for {
    PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
    ArgumentSpan(depLabel, argWords) <- args
    if !depLabel.equals("V")
    qaInfo <- qas
    questionLabel <- qaInfo.questionFirstWordIfNew.toList
    if argWords.forall(word =>
      qaInfo.arcs.contains(pred.head.index, word.index) || qaInfo.arcs.contains(word.index, pred.head.index))
  } yield (depLabel, questionLabel)

  val validQuestionNum = qas.filter(_.questionIsValid).size
  val invalidQuestionNum = qas.filterNot(_.questionIsValid).size
  val validQuestionProportion = validQuestionNum.toDouble / qas.size
  val validAnswerNum = qas.filter(_.answerIsValid).size
  val invalidAnswerNum = qas.filterNot(_.answerIsValid).size
  val validAnswerProportion = validAnswerNum.toDouble / qas.size
  val validQAPairNum = qas.filter(_.qaPairIsValid).size
  val invalidQAPairNum = qas.filterNot(_.qaPairIsValid).size
  val validQAPairProportion = validQAPairNum.toDouble / qas.size
}

object AggregatedQAInfo {
  // could be assignment ID, HIT ID, HIT type ID...
  def aggregateBy(
    factorNames: List[String],
    extractFactors: (QAInfo => List[String]),
    infos: Iterable[QAInfo]
  ): List[AggregatedQAInfo] = {
    infos.groupBy(extractFactors).map {
      case (factors, infos) => AggregatedQAInfo(factorNames, factors, infos.toList)
    }.toList
  }

  def makeTSV(aggInfos: List[AggregatedQAInfo]) = {
    val factorHeaders = aggInfos.head.tagNames.mkString("\t")
    val header = s"$factorHeaders\tquestionOverlapCount\tquestionOverlapProportion\tquestionOverlapPerQA\t" +
      "answerOverlapCount\tanswerOverlapProportion\tanswerOverlapPerQA\t" +
      "coveredLabelProportion\tsomeWordCoveredLabelProportion\tallWordsCoveredLabelProportion\n"
    header + aggInfos.map { case aggInfo@AggregatedQAInfo(_, factors, infos) =>
      import aggInfo._
      val factorValues = factors.mkString("\t")
      s"$factorValues\t$questionOverlapCount\t$questionOverlapProportion\t$questionOverlapPerQA\t" +
        s"$answerOverlapCount\t$answerOverlapProportion\t$answerOverlapPerQA\t" +
        s"$coveredLabelProportion\t$someWordCoveredLabelProportion\t$allWordsCoveredLabelProportion"
    }.mkString("\n")
  }
}
