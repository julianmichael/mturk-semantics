package mts.analysis

import mts.datasets.conll._
import mts.util._
import mts.util.LowerCaseStrings._
import mts.language._
import TreeInduction.Index

import mts.experiments.expE.renderSpan
import mts.experiments.expG.AlignedManualQARecord
// import mts.experiments.expG.KeywordedQAPair
import mts.experiments.expG.AlignedKeywordedQAPair

class QAPairAnalysis(
  goldAnnotations: Map[CoNLLSentencePath, AlignedManualQARecord], // TODO change all analysis to accommodate
  turkAnnotations: Map[CoNLLSentencePath, AnnotatedSentence],
  makeHypergraph: AnnotatedSentence => DirectedHypergraph[Word, List[String]]//,
  // makeTree: AnnotatedSentence => DependencyTree[Index, Unit]
) {

  val goldQuestionWordsHist = {
    val qiSizes = for {
      record <- goldAnnotations.values
      group <- record.qaGroups
      qa <- group
    } yield qa.questionIndices.size
    Scorer[Int, Int](qiSizes)
  }

  val turkQuestionWordsHist = {
    val qiSizes = for {
      as <- turkAnnotations.values
      aqa <- as.alignedQAPairs
    } yield aqa.questionIndices.size
    Scorer[Int, Int](qiSizes)
  }

  def statsFor(
    matches: (CoNLLSentencePath, AlignedQuestionAnswerPair, AlignedKeywordedQAPair) => Boolean
  ) = goldAnnotations.keys.toVector.map { k =>

    val truePositives = for {
      qaPair <- turkAnnotations(k).alignedQAPairs
    } yield if(goldAnnotations(k).qaGroups.flatten.exists(matches(k, qaPair, _))) 1 else 0

    val positiveTrues = for {
      group <- goldAnnotations(k).qaGroups
    } yield if(group.exists(akqa => turkAnnotations(k).alignedQAPairs.exists(aqa => matches(k, aqa, akqa)))) 1 else 0

    PrecisionRecall(
      numPredicted = turkAnnotations(k).alignedQAPairs.size,
      numGold = goldAnnotations(k).qaGroups.size,
      numCorrect = truePositives.sum,
      numCovered = positiveTrues.sum)
  }

  def offBy[A](i: Int)(s1: Set[A], s2: Set[A]) = ((s1 union s2) -- (s1 intersect s2)).size <= i
  def anyOverlap[A](s1: Set[A], s2: Set[A]) = (s1 intersect s2).size >= 1
  def oboMatch[A](s1: Set[A], s2: Set[A]) = offBy(1)(s1, s2)

  def answerExactMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: AlignedKeywordedQAPair) =
    ref.answerIndices == alignedQAPair.qaPair.answer
  def answerOboMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: AlignedKeywordedQAPair) =
    oboMatch(ref.answerIndices, alignedQAPair.qaPair.answer)

  def answerExact1AndFirstQWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: AlignedKeywordedQAPair) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref match {
      case AlignedKeywordedQAPair(refQ, _, refQIndices, refA) =>
        refA == alignedQAPair.qaPair.answer && (refQIndices.size == 0 ||
          anyOverlap(alignedQAPair.questionIndices, refQIndices)) && (
          alignedQAPair.qaPair.question.takeWhile(_ != ' ').toLowerCase == refQ.takeWhile(_ != ' ').toLowerCase)
    }
  }
  def answerExact1QWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: AlignedKeywordedQAPair) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref match {
      case AlignedKeywordedQAPair(refQ, _, refQIndices, refA) =>
        refA == alignedQAPair.qaPair.answer && (refQIndices.size == 0 ||
          anyOverlap(alignedQAPair.questionIndices, refQIndices))
    }
  }
  def answerObo1QWordMatch(path: CoNLLSentencePath, alignedQAPair: AlignedQuestionAnswerPair, ref: AlignedKeywordedQAPair) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    ref match {
      case AlignedKeywordedQAPair(refQ, _, refQIndices, refA) =>
        oboMatch(refA, alignedQAPair.qaPair.answer) && (refQIndices.size == 0 ||
          anyOverlap(alignedQAPair.questionIndices, refQIndices))
    }
  }

  lazy val answerExactStats = statsFor(answerExactMatch)
  lazy val answerFuzzyStats = statsFor(answerOboMatch)

  lazy val answerExact1AndFirstQStats = statsFor(answerExact1AndFirstQWordMatch)
  lazy val answerExact1QStats = statsFor(answerExact1QWordMatch)
  lazy val answerFuzzy1QStats = statsFor(answerObo1QWordMatch)

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder
  import scala.concurrent.duration._

  @memoize(maxSize = 50, expiresAfter = 1 hour)
  def getSaturatedGoldHypergraph(path: CoNLLSentencePath): DirectedHypergraph[Word, List[String]] = {
    val sentence = FileManager.getCoNLLSentence(path).get
    val edges = goldAnnotations(path).qaGroups.flatten.groupBy(akqa => (akqa.keywordIndex, akqa.answerIndices)).map {
      case ((keywordIndex, answer), akqas) =>
        Hyperedge(
          akqas.map(_.question),
          sentence.words(keywordIndex),
          sentence.words.filter(w => answer.contains(w.index)).toSet)
    }.toSet
    val nodes = sentence.words.toSet
    DirectedHypergraph(nodes, edges)
  }

  @memoize(maxSize = 50, expiresAfter = 1 hour)
  def getGoldHypergraph(path: CoNLLSentencePath): DirectedHypergraph[Word, List[String]] =
    makeHypergraph(
      goldAnnotations(path) match {
        case AlignedManualQARecord(path, sentence, groups) =>
          AnnotatedSentence(
            path, groups.flatten.map {
              case AlignedKeywordedQAPair(q, _, qis, a) => AlignedQuestionAnswerPair(QuestionAnswerPair(q, a), qis)
            })
      })

  @memoize(maxSize = 50, expiresAfter = 1 hour)
  def getTurkHypergraph(path: CoNLLSentencePath): DirectedHypergraph[Word, List[String]] =
    makeHypergraph(turkAnnotations(path))

  // the pred itself, discourse markers, negations, and auxiliaries we don't care about
  def labelIsIrrelevant(l: String) = {
    l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD")
  }
  val copulas = Set("be", "am", "are", "is", "was", "were")

  def getHypergraphArgStats(
    sentence: CoNLLSentence,
    annotationHypergraph: DirectedHypergraph[Word, List[String]],
    argsMatch: (Set[Word], Set[Word]) => Boolean) = {
    val trues = for {
      pas <- sentence.predicateArgumentStructures.toList
      if !copulas.contains(pas.pred.head.token)
      arg <- pas.arguments.toList
      if !labelIsIrrelevant(arg.label)
    } yield (pas.pred, arg)

    val positiveTrues = for {
      (pred, arg) <- trues
    } yield {
      val forwardMatch = annotationHypergraph.edges.exists(e =>
        e.target == pred.head && argsMatch(arg.words.toSet, e.source)
      )
      val backwardMatch = annotationHypergraph.edges.exists(e =>
        arg.words.contains(e.target) && e.source.contains(pred.head)
      )
      if(!forwardMatch && !backwardMatch) {
        println(s"""Missed dependency; ${pred.head.token}: ${renderSpan(sentence, arg.words.map(_.index).toSet)}""")
      }
      if(forwardMatch || backwardMatch) 1 else 0
    }

    val truePositives = for {
      Hyperedge(_, target, source) <- annotationHypergraph.edges.toList
    } yield {
      val forwardMatch = trues.exists {
        case (pred, arg) => target == pred.head && argsMatch(arg.words.toSet, source)
      }
      val backwardMatch = trues.exists {
        case (pred, arg) => arg.words.contains(target) && source.contains(pred.head)
      }
      if(forwardMatch || backwardMatch) 1 else 0
    }

    PrecisionRecall(
      numPredicted = annotationHypergraph.edges.size,
      numGold = trues.size,
      numCorrect = truePositives.sum,
      numCovered = positiveTrues.sum)
  }
    // sentence.predicateArgumentStructures.collect {
    //   // ignore copula predicates and irrelevant arg labels
    //   case PredicateArgumentStructure(Predicate(head, _, _), goldArgs) if !copulas.contains(head.token) =>
    //     val goldSpans = goldArgs.filterNot(arg => labelIsIrrelevant(arg.label)).map(_.words.map(_.index).toSet)
    //     val predictedSpans = annotationHypergraph.edges.filter(_.target == head).map(_.source.map(_.index))
    //     val correct = predictedSpans.filter(pSpan => goldSpans.exists(argsMatch(_, pSpan)))
    //     val covered = goldSpans.filter(gSpan => predictedSpans.exists(argsMatch(_, gSpan)))
    //     PrecisionRecall(
    //       numPredicted = predictedSpans.size,
    //       numGold = goldSpans.size,
    //       numCorrect = correct.size,
    //       numCovered = covered.size)
    // }

  def saturatedGoldHypergraphArgStats(argsMatch: (Set[Word], Set[Word]) => Boolean) =
    goldAnnotations.keys.toList.map { path =>
      val hypergraph = getSaturatedGoldHypergraph(path)
      val sentence = FileManager.getCoNLLSentence(path).get
      getHypergraphArgStats(sentence, hypergraph, argsMatch)
    }

  def goldHypergraphArgStats(argsMatch: (Set[Word], Set[Word]) => Boolean) =
    goldAnnotations.keys.toList.map { path =>
      val hypergraph = getGoldHypergraph(path)
      val sentence = FileManager.getCoNLLSentence(path).get
      getHypergraphArgStats(sentence, hypergraph, argsMatch)
    }

  def turkHypergraphArgStats(argsMatch: (Set[Word], Set[Word]) => Boolean) =
    turkAnnotations.toList.map {
      case (path, annotatedSentence) =>
        val hypergraph = getTurkHypergraph(path)
        val sentence = FileManager.getCoNLLSentence(path).get
        getHypergraphArgStats(sentence, hypergraph, argsMatch)
    }


  // def treeStats(
  //   sentence: CoNLLSentence,
  //   annotationTree: DependencyTree[Index, Unit]
  // )
  def goldStatsString = s"""
Number of sentence words in question:
${histogramString(goldQuestionWordsHist)}

Saturated SRL Hypergraph argument spans (exact match):
${statString(saturatedGoldHypergraphArgStats(_ == _).reduce(_ aggregate _))}
Saturated SRL Hypergraph argument spans (off-by-one match):
${statString(saturatedGoldHypergraphArgStats(offBy(1)).reduce(_ aggregate _))}
Saturated SRL Hypergraph argument spans (any overlap):
${statString(saturatedGoldHypergraphArgStats(anyOverlap).reduce(_ aggregate _))}
SRL Hypergraph argument spans (exact match):
${statString(goldHypergraphArgStats(_ == _).reduce(_ aggregate _))}
SRL Hypergraph argument spans (off-by-one match):
${statString(goldHypergraphArgStats(offBy(1)).reduce(_ aggregate _))}
SRL Hypergraph argument spans (any overlap):
${statString(goldHypergraphArgStats(anyOverlap).reduce(_ aggregate _))}
"""

  def allStatsString = s"""
Number of sentence words in question:
${histogramString(turkQuestionWordsHist)}
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
