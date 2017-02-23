package mts.analysis

import mts.language._
import mts.conll._
import mts.util._
import LowerCaseStrings._

//TODO move this out of exp E
import mts.experiments.expE.renderSpan

object DirectedHypergraphInduction {

  def getPASHypergraph(sentence: CoNLLSentence): DirectedHypergraph[CoNLLWord, String] = {
    val nodes = sentence.words.toSet
    val hyperedges = for {
      PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
      ArgumentSpan(label, argWords) <- args
    } yield Hyperedge(label, pred.head, argWords.toSet)
    DirectedHypergraph(nodes, hyperedges.toSet)
  }

  def getCorrelations(annotation: AnnotatedSentence) = {
    import scala.collection.mutable
    val cooccurrence = mutable.Map.empty[(Int, Int), List[AlignedQuestionAnswerPair]].withDefaultValue(Nil)
    val qCounts = Counter[Int]
    val aCounts = Counter[Int]
    val sentence = annotation.sentence
    val scores = for {
      alignedQAPair @ AlignedQuestionAnswerPair(QuestionAnswerPair(_, answerIndices), questionIndices) <- annotation.alignedQAPairs
      qi <- questionIndices
      ai <- answerIndices
    } yield {
      cooccurrence.put((qi, ai), alignedQAPair :: cooccurrence((qi, ai)))
      qCounts.add(qi)
      aCounts.add(ai)
    }
    val total = cooccurrence.values.map(_.size).sum
    val totalQ = qCounts.sum
    val totalA = aCounts.sum

    val correlations = cooccurrence.iterator.map {
      case ((qi, ai), questions) =>
        val prob = questions.size.toDouble / total
        val qProb = qCounts(qi).toDouble / totalQ
        val aProb = aCounts(ai).toDouble / totalA
        val corr = prob - (qProb * aProb)
        (qi, ai, corr, questions)
    }.toList
    correlations
  }

  def getHypergraph(annotation: AnnotatedSentence) = {
    val sentence = annotation.sentence
    val scores = getCorrelations(annotation)
    val processedScores = scores.map {
      case (qi, ai, mi, qaPairs) =>
        val questions = qaPairs.map(_.qaPair.question)
        val protoAnswerSpan = majorities(qaPairs.map(_.qaPair.answer))
        (qi, ai, mi, protoAnswerSpan, questions)
    }
    val hyperedges = for {
      ((qi, aSpan), entries) <- processedScores.groupBy(t => (t._1, t._4)).toVector
      maxCorr = entries.map(_._3).toList.max
      questions = entries.map(_._5).flatten.toSet.toList
    } yield Hyperedge((questions, maxCorr), qi, aSpan)
    val nodes = sentence.words.map(_.index).toSet
    DirectedHypergraph(nodes, hyperedges.toSet)
  }
}
