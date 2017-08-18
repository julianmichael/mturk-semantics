package example.emnlp2017

import nlpdata.datasets.wiktionary.Inflections
import turksem.IsStopword
import turksem.qamr._
import turksem.util._

case class QAMRNode(begin: Int, end: Int) {
  def isProperSubspanOf(other: QAMRNode): Boolean =
    ((end - begin) < (other.end - other.begin)) &&
      begin >= other.begin && end <= other.end
  def overlapsWith(other: QAMRNode): Boolean =
    (other.begin <= this.end && other.end >= this.begin)
  def union(other: QAMRNode): QAMRNode =
    QAMRNode(math.min(this.begin, other.begin), math.max(this.end, other.end))
}
case class QAMRPAS(pred: QAMRNode, args: Map[QAMRNode, Set[String]]) {
  def addArg(question: Option[String], answer: QAMRNode): QAMRPAS = {
    val qs = args.get(answer).getOrElse(Set.empty[String])
    val newQs = question.fold(qs)(qs + _)
    QAMRPAS(pred, args.updated(answer, newQs))
  }
}
case class QAMRGraph(paStructures: Map[QAMRNode, QAMRPAS])

object StructureInduction {

  def isBetween(
    query: Int,
    references: (Int, Int)
  ): Boolean = {
    val (lower, higher) =
      if(references._1 < references._2) references
      else (references._2, references._1)
    query >= lower && query <= higher
  }

  case class Edge(qOpt: Option[String], source: QAMRNode, target: QAMRNode)

  def isNewEdgeProjective(
    curGraph: Map[QAMRNode, QAMRPAS],
    newEdge: Edge
  ): Boolean = {
    curGraph.forall { case (pred, QAMRPAS(_, args)) =>
      args.forall { case (arg, _) =>
        val source = newEdge.source.begin
        val target = newEdge.target.begin
        val predArgPair = (pred.begin, arg.begin)
        if(source == pred.begin || target == pred.begin ||
             source == arg.begin || target == arg.begin) true
        else if(isBetween(source, predArgPair)) isBetween(target, predArgPair)
        else !isBetween(target, predArgPair)
      }
    }
  }

  def breakIntoContiguous(s: Set[Int]): List[Set[Int]] = {
    if(s.isEmpty) Nil else {
      val min = s.min
      var max = s.min + 1
      while(s.contains(max)) {
        max = max + 1
      }
      val firstInterval = (min until max).toSet
      firstInterval :: breakIntoContiguous(s -- firstInterval)
    }
  }

  def getMinimalContiguousSpans(
    tokens: Vector[String], qas: List[SourcedQA[SentenceId]])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[Set[Int]] = {
    val allContiguousSpans = qas.flatMap { sqa =>
      val qSpan = getWordsInQuestion(tokens, tokenize(sqa.question));
      (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
    }

    allContiguousSpans.filter(span =>
      !allContiguousSpans.exists(otherSpan =>
        otherSpan.subsetOf(span) && !span.subsetOf(otherSpan)
      )
    ).toSet

  }

  class GraphInducer(
    tokens: Vector[String], qas: List[SourcedQA[SentenceId]])(
    implicit inflections: Inflections,
    isStopword: IsStopword) {

    val allContiguousSpans = qas.flatMap { sqa =>
      val qSpan = getWordsInQuestion(tokens, tokenize(sqa.question));
      (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
    }

    val minimalContiguousSpans = getMinimalContiguousSpans(tokens, qas)

    val minimalSpanQuestionAppearanceCounts = Scorer[Set[Int], Int](
      qas.flatMap { qa =>
        val qSpan = getWordsInQuestion(tokens, tokenize(qa.question))
        breakIntoContiguous(qSpan).filter(minimalContiguousSpans.contains)
      }
    )

    val minimalSpanAllAppearanceCounts = Scorer[Set[Int], Int](
      allContiguousSpans.filter(minimalContiguousSpans.contains)
    )

    def spanPredicateness(s: Set[Int]): Double =
      minimalSpanQuestionAppearanceCounts(s).toDouble / minimalSpanAllAppearanceCounts(s)

    val spansByPredicateness = {
      val spanVec = minimalContiguousSpans.toVector
      spanVec.zip(spanVec.map(spanPredicateness))
        .sortBy(-_._2)
        .map(_._1)
    }

    val qasByPred = qas.groupBy { qa =>
      spansByPredicateness.iterator.filter {
        span => span.forall(getWordsInQuestion(tokens, tokenize(qa.question)).contains)
      }.nextOption
    }.collect {
      case (Some(pred), qas) => pred -> qas
    }

    val edges = for {
      predSpan <- spansByPredicateness
      qa <- qasByPred.get(predSpan).getOrElse(Nil)
      qTokens = getWordsInQuestion(tokens, tokenize(qa.question))
      answer <- qa.answers
      argSpan <- spansByPredicateness
      if spanPredicateness(predSpan) > spanPredicateness(argSpan)
      argInAnswer = argSpan.forall(answer.contains)
      if argSpan.forall(qTokens) || argInAnswer
      qOpt = if(argInAnswer) Some(qa.question) else None
    } yield Edge(qOpt, QAMRNode(predSpan.min, predSpan.max), QAMRNode(argSpan.min, argSpan.max))

    // agg edges together and ignore non-projective ones

    val paStructures = edges.foldLeft(Map.empty[QAMRNode, QAMRPAS]) {
      case (acc, e @ Edge(qOpt, source, target)) =>
        if(isNewEdgeProjective(acc, e)) {
          acc.updated(
            source,
            acc.get(source) match {
              case None => QAMRPAS(source, Map(target -> qOpt.toSet))
              case Some(pas) => pas.addArg(qOpt, target)
            }
          )
        } else acc
    }

    val standardQAMRGraph = QAMRGraph(paStructures)

    val srlStyleQAMRGraph = {
      val predArgStructures = for {
        predSpan <- spansByPredicateness
        qas <- qasByPred.get(predSpan).toList
      } yield {
        val predNode = QAMRNode(predSpan.min, predSpan.max)
        val minimalArgumentSpans = getMinimalContiguousSpans(tokens, qas)
          .filterNot(_.contains(predNode.begin)) // filter out pred span
          .filterNot(span =>
          minimalContiguousSpans.exists(mspan =>
            spanPredicateness(mspan) > spanPredicateness(span) &&
              mspan.forall(span.contains) // filter out args containing more predicatey spans
          )
        )
        val edges = for {
          qa <- qas
          qTokens = getWordsInQuestion(tokens, tokenize(qa.question))
          answer <- qa.answers
          argSpan <- minimalArgumentSpans
          argInAnswer = argSpan.forall(answer.contains)
          if argSpan.forall(qTokens) || argInAnswer
          qOpt = if(argInAnswer) Some(qa.question) else None
        } yield Edge(qOpt, predNode, QAMRNode(argSpan.min, argSpan.max))

        val args = edges.groupBy(_.target).map {
          case (argNode, edges) => argNode -> edges.flatMap(_.qOpt).toSet
        }
        predNode -> QAMRPAS(predNode, args)
      }
      QAMRGraph(predArgStructures.toMap)
    }

    val answerOnlySRLStyleQAMRGraph = {
      val predArgStructures = for {
        predSpan <- spansByPredicateness
        qas <- qasByPred.get(predSpan).toList
      } yield {
        val predNode = QAMRNode(predSpan.min, predSpan.max)
        val maximalAnswerNodes = {
          val answerNodes = for {
            qa <- qas
            answer <- qa.answers
          } yield QAMRNode(answer.min, answer.max)
          val nonSubstringAnswers = answerNodes
            .filter(a => !answerNodes.exists(a.isProperSubspanOf))
            .toSet
          val expandedAnswerSpans = nonSubstringAnswers.iterator.map { answer =>
            var cur = answer
            var next = answer
            do {
              cur = next
              for(a <- nonSubstringAnswers) {
                if(next.overlapsWith(a)) next = next.union(a)
              }
            } while(next != cur);
            next
          }.toSet
          expandedAnswerSpans
        }
        val args = for {
          argSpan <- maximalAnswerNodes
          // questions = qas.filter(_.answers.exists(a => a.min >= argSpan.begin && a.max <= argSpan.end)).map(_.question)
          questions = List.empty[String]
        } yield argSpan -> questions.toSet

        predNode -> QAMRPAS(predNode, args.toMap)
      }
      QAMRGraph(predArgStructures.toMap)
    }
  }

  def renderPredIDSeq(tokens: Vector[String], graph: QAMRGraph): String = {
    val predIndices = graph.paStructures.keys.map(_.begin).toSet
    val tags = tokens.indices.map { index =>
      if(predIndices.contains(index)) "V" else "O"
    }
    tokens.mkString(" ") + " ||| " + tags.mkString(" ")
  }

  def renderArgTaggedSeqs(tokens: Vector[String], graph: QAMRGraph): List[String] = {
    graph.paStructures.iterator.map { case (pred, QAMRPAS(_, args)) =>
      def getTag(index: Int): String =
        if(index == pred.begin) "B-V"
        else if(index > pred.begin && index <= pred.end) "I-V"
        else if(args.keys.exists(a => index == a.begin)) "B-A"
        else if(args.keys.exists(a => index > a.begin && index <= a.end)) "I-A"
        else "O"
      val tags = tokens.indices.map(getTag)
      pred.begin + "\t" + tokens.mkString(" ") + " ||| " + tags.mkString(" ")
    }.toList
  }
}
