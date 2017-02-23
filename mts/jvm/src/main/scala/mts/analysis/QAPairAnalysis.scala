package mts.analysis

import mts.conll._
import mts.util._
import mts.language._
import TreeInduction.Index

class QAPairAnalysis(
  goldAnnotations: Map[CoNLLSentencePath, AnnotatedSentence],
  turkAnnotations: Map[CoNLLSentencePath, AnnotatedSentence],
  makeHypergraph: AnnotatedSentence => DirectedHypergraph[CoNLLWord, List[String]]//,
  // makeTree: AnnotatedSentence => DependencyTree[Index, Unit]
) {

  def questionWordsHist(annotations: Map[CoNLLSentencePath, AnnotatedSentence]) = {
    val qiSizes = for {
      as <- annotations.values
      aqa <- as.alignedQAPairs
    } yield aqa.questionIndices.size
    Scorer[Int, Int](qiSizes)
  }

  def correctFor(
    guesser: AnnotatedSentence,
    reference: AnnotatedSentence,
    matches: (CoNLLSentencePath, AlignedQuestionAnswerPair, List[AlignedQuestionAnswerPair]) => Boolean
  ) = {
    val scores = for {
      qaPair <- guesser.alignedQAPairs
      ref = reference.alignedQAPairs
      score = if(matches(guesser.path, qaPair, ref)) 1 else 0
    } yield score
    scores.sum
  }

  def statsFor(
    guesser: Map[CoNLLSentencePath, AnnotatedSentence],
    reference: Map[CoNLLSentencePath, AnnotatedSentence],
    matches: (CoNLLSentencePath, AlignedQuestionAnswerPair, List[AlignedQuestionAnswerPair]) => Boolean
  ) = reference.keys.toVector.map(k =>
    PrecisionRecall(
      numPredicted = guesser(k).alignedQAPairs.size,
      numGold = reference(k).alignedQAPairs.size,
      numCorrect = correctFor(guesser(k), reference(k), matches),
      numCovered = correctFor(reference(k), guesser(k), matches))
  )

  def offBy[A](i: Int)(s1: Set[A], s2: Set[A]) = ((s1 union s2) -- (s1 intersect s2)).size <= i
  def anyOverlap[A](s1: Set[A], s2: Set[A]) = (s1 intersect s2).size >= 1
  def oboMatch[A](s1: Set[A], s2: Set[A]) = offBy(1)(s1, s2)

  def answerExactMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: List[AlignedQuestionAnswerPair]) =
    ref.map(_.qaPair.answer).contains(alignedQAPair.qaPair.answer)
  def answerOboMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: List[AlignedQuestionAnswerPair]) =
    ref.map(_.qaPair.answer).exists(oboMatch(_, alignedQAPair.qaPair.answer))

  def answerExact1AndFirstQWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: List[AlignedQuestionAnswerPair]) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref.exists {
      case AlignedQuestionAnswerPair(QuestionAnswerPair(refQ, refA), refQIndices) =>
        refA == alignedQAPair.qaPair.answer && (refQIndices.size == 0 ||
          alignedQAPair.questionIndices.intersect(refQIndices).size > 0) && (
          alignedQAPair.qaPair.question.takeWhile(_ != ' ').toLowerCase == refQ.takeWhile(_ != ' ').toLowerCase)
    }
  }
  def answerExact1QWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: List[AlignedQuestionAnswerPair]) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref.exists {
      case AlignedQuestionAnswerPair(QuestionAnswerPair(refQ, refA), refQIndices) =>
        refA == alignedQAPair.qaPair.answer && (refQIndices.size == 0 ||
          alignedQAPair.questionIndices.intersect(refQIndices).size > 0)
    }
  }
  def answerObo1QWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: List[AlignedQuestionAnswerPair]) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref.exists {
      case AlignedQuestionAnswerPair(QuestionAnswerPair(refQ, refA), refQIndices) =>
        oboMatch(refA, alignedQAPair.qaPair.answer) && (refQIndices.size == 0 ||
          alignedQAPair.questionIndices.intersect(refQIndices).size > 0)
    }
  }

  lazy val answerExactStats = statsFor(turkAnnotations, goldAnnotations, answerExactMatch)
  lazy val answerFuzzyStats = statsFor(turkAnnotations, goldAnnotations, answerOboMatch)

  lazy val answerExact1AndFirstQStats = statsFor(turkAnnotations, goldAnnotations, answerExact1AndFirstQWordMatch)
  lazy val answerExact1QStats = statsFor(turkAnnotations, goldAnnotations, answerExact1QWordMatch)
  lazy val answerFuzzy1QStats = statsFor(turkAnnotations, goldAnnotations, answerObo1QWordMatch)

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder
  import scala.concurrent.duration._

  // @memoize(maxSize = 1000, expiresAfter = 1 hour)
  // def getGoldHypergraph(path: CoNLLSentencePath): DirectedHypergraph[CoNLLWord, List[String]] =
  //   makeHypergraph(goldAnnotations(path))

  @memoize(maxSize = 1000, expiresAfter = 1 hour)
  def getTurkHypergraph(path: CoNLLSentencePath): DirectedHypergraph[CoNLLWord, List[String]] =
    makeHypergraph(turkAnnotations(path))

  // @memoize(maxSize = 1000, expiresAfter = 1 hour)
  // def getPASHypergraph(path: CoNLLSentencePath): DirectedHypergraph[CoNLLWord, String] =
  //   DirectedHypergraphInduction
  //     .getPASHypergraph(FileManager.getCoNLLSentence(path).get)
  //     .filterEdges(_.label != "V")

  def getHypergraphArgStats(
    sentence: CoNLLSentence,
    annotationHypergraph: DirectedHypergraph[CoNLLWord, List[String]],
    argsMatch: (Set[Int], Set[Int]) => Boolean) =
    sentence.predicateArgumentStructures.map {
      case PredicateArgumentStructure(Predicate(head, _, _), goldArgs) =>
        val goldSpans = goldArgs.filter(_.label != "V").map(_.words.map(_.index).toSet)
        val predictedSpans = annotationHypergraph.edges.filter(_.target == head).map(_.source.map(_.index))
        val correct = predictedSpans.filter(pSpan => goldSpans.exists(argsMatch(_, pSpan)))
        val covered = goldSpans.filter(gSpan => predictedSpans.exists(argsMatch(_, gSpan)))
        PrecisionRecall(
          numPredicted = predictedSpans.size,
          numGold = goldSpans.size,
          numCorrect = correct.size,
          numCovered = covered.size)
    }

  def turkHypergraphArgStats(argsMatch: (Set[Int], Set[Int]) => Boolean) =
    turkAnnotations.toList.flatMap {
      case (path, annotatedSentence) =>
        val hypergraph = getTurkHypergraph(path)
        val sentence = FileManager.getCoNLLSentence(path).get
        getHypergraphArgStats(sentence, hypergraph, argsMatch)
    }


  // def treeStats(
  //   sentence: CoNLLSentence,
  //   annotationTree: DependencyTree[Index, Unit]
  // )

  def allStatsString = s"""
Number of sentence words in question:
Gold:
${histogramString(questionWordsHist(goldAnnotations))}
Given:
${histogramString(questionWordsHist(turkAnnotations))}
Answer spans (off-by-one match):
${statString(answerFuzzyStats.reduce(_ aggregate _))}
Answer spans (exact match):
${statString(answerExactStats.reduce(_ aggregate _))}
Answer spans (off-by-one match) and one question word:
${statString(answerFuzzy1QStats.reduce(_ aggregate _))}
Answer spans (exact match) and one question word:
${statString(answerExact1QStats.reduce(_ aggregate _))}
Answer spans (exact match) and one question word + first word in question:
${statString(answerExact1AndFirstQStats.reduce(_ aggregate _))}

SRL Hypergraph argument spans (exact match):
${statString(turkHypergraphArgStats(_ == _).reduce(_ aggregate _))}
SRL Hypergraph argument spans (off-by-one match):
${statString(turkHypergraphArgStats(offBy(1)).reduce(_ aggregate _))}
SRL Hypergraph argument spans (any overlap):
${statString(turkHypergraphArgStats(anyOverlap).reduce(_ aggregate _))}
"""
}
