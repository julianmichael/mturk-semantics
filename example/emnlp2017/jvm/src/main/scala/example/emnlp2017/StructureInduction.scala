package example.emnlp2017

import cats.Monoid
import cats.Show
import cats.data.State
import cats.implicits._

import nlpdata.datasets.wiktionary.Inflections

import qamr._
import qamr.util.IsStopword
import qamr.example.SentenceId
import turksem.util._

import monocle.macros._

case class QAMRNode(begin: Int, end: Int) {
  def isProperSubspanOf(other: QAMRNode): Boolean =
    ((end - begin) < (other.end - other.begin)) &&
      begin >= other.begin && end <= other.end
  def overlapsWith(other: QAMRNode): Boolean =
    (other.begin <= this.end && other.end >= this.begin)
  def union(other: QAMRNode): QAMRNode =
    QAMRNode(math.min(this.begin, other.begin), math.max(this.end, other.end))
}
case class QAMRPAS[Label](pred: QAMRNode, args: Map[QAMRNode, Label]) {
  def mapLabels[B](f: Label => B): QAMRPAS[B] = QAMRPAS(
    pred, args.map { case (node, label) => node -> f(label) })

  def addArg(answer: QAMRNode, arg: Label)(implicit m: Monoid[Label]): QAMRPAS[Label] = {
    val qs = args.get(answer).getOrElse(m.empty)
    QAMRPAS(pred, args.updated(answer, qs |+| arg))
  }
}
case class QAMRGraph[Label](paStructures: Map[QAMRNode, QAMRPAS[Label]]) {
  def mapLabels[B](f: Label => B): QAMRGraph[B] = QAMRGraph(
    paStructures.map { case (node, pas) => node -> pas.mapLabels(f) }
  )

  def roots = paStructures.collect {
    case (pred, pas) if !paStructures.exists(pair => pair._2.args.keySet.contains(pred)) => pred
  }.toVector

  private def hasCycleAux(node: QAMRNode, ancestors: Set[QAMRNode]): Boolean = {
    ancestors.contains(node) || paStructures.get(node).nonEmptyAnd(pas =>
      pas.args.keys.exists(hasCycleAux(_, ancestors + node))
    )
  }
  def hasCycle: Boolean = roots.exists(hasCycleAux(_, Set.empty[QAMRNode]))

  def stringFormAux(
    tokens: Vector[String],
    node: QAMRNode
  )(implicit labelShow: Show[Label]): State[Set[QAMRNode], Vector[String]] = {
    for {
      coveredNodes <- State.get[Set[QAMRNode]]
      children <- if(coveredNodes.contains(node)) {
        State.pure[Set[QAMRNode], Vector[String]](Vector.empty)
      } else {
        State.modify[Set[QAMRNode]](_ + node).flatMap(_ =>
          paStructures.get(node).fold(State.pure[Set[QAMRNode], Vector[String]](Vector.empty))(pas =>
            pas.args.toVector.sortBy(_._1.begin).map { case (arg, label) =>
              stringFormAux(tokens, arg).map((":" + label.show) +: _)
            }.sequence[State[Set[QAMRNode], ?], Vector[String]].map(_.flatten)
          )
        )
      }
    } yield ("(" +: (node.begin to node.end).map(tokens)).toVector ++ children :+ ")"
  }
  def stringForm(
    tokens: Vector[String]
  )(implicit labelShow: Show[Label]): Vector[String] = {
    roots
      .sortBy(_.begin)
      .map(stringFormAux(tokens, _))
      .sequence[State[Set[QAMRNode], ?], Vector[String]].map(_.flatten)
      .runA(Set.empty[QAMRNode]).value
  }

  @Lenses case class PrettyPrintState(
    coveredNodes: Set[QAMRNode],
    curIndent: Int)
  object PrettyPrintState {
    def init = PrettyPrintState(Set.empty, 0)
    def addNode(n: QAMRNode) = PrettyPrintState.coveredNodes.modify(_ + n)
    def indent = PrettyPrintState.curIndent.modify(_ + 1)
    def unindent = PrettyPrintState.curIndent.modify(_ - 1)
  }
  def prettyStringAux(
    tokens: Vector[String],
    node: QAMRNode
  )(implicit labelShow: Show[Label]): State[PrettyPrintState, String] = {
    for {
      s <- State.get[PrettyPrintState]
      children <- if(s.coveredNodes.contains(node)) {
        State.pure[PrettyPrintState, String]("")
      } else {
        State.modify[PrettyPrintState](PrettyPrintState.addNode(node) andThen PrettyPrintState.indent).flatMap(_ =>
          paStructures.get(node).fold(State.pure[PrettyPrintState, String](""))(pas =>
            pas.args.toVector.sortBy(_._1.begin).map { case (arg, label) =>
              prettyStringAux(tokens, arg).flatMap(argStr =>
                State.get[PrettyPrintState].map(s =>
                  ("\n" + ("\t" * s.curIndent) + ":" + label.show) + " " + argStr
                )
              )
            }.sequence[State[PrettyPrintState, ?], String].map(" " + _.mkString)
          )
        ).flatMap(x => State.modify[PrettyPrintState](PrettyPrintState.unindent).as(x))
      }
    } yield "(" + " " + (node.begin to node.end).map(tokens).mkString(" ") + children + " " + ")"
  }
  def prettyString(
    tokens: Vector[String]
  )(implicit labelShow: Show[Label]): String = {
    roots
      .sortBy(_.begin)
      .map(prettyStringAux(tokens, _))
      .sequence[State[PrettyPrintState, ?], String].map(_.mkString("\n"))
      .runA(PrettyPrintState.init).value
  }
}

case class Edge(qOpt: Option[String], source: QAMRNode, target: QAMRNode)

class StructureInduction(
  implicit inflections: Inflections
) {

  def isBetween(
    query: Int,
    references: (Int, Int)
  ): Boolean = {
    val (lower, higher) =
      if(references._1 < references._2) references
      else (references._2, references._1)
    query >= lower && query <= higher
  }

  def isNewEdgeProjective[Label](
    curGraph: Map[QAMRNode, QAMRPAS[Label]],
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
      val qSpan = getWordsInQuestion(tokens, Tokenizer.tokenize(sqa.question));
      (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
    }

    allContiguousSpans.filter(span =>
      !allContiguousSpans.exists(otherSpan =>
        otherSpan.subsetOf(span) && !span.subsetOf(otherSpan)
      )
    ).toSet

  }

  def renderPredIDSeq[Label](tokens: Vector[String], graph: QAMRGraph[Label]): String = {
    val predIndices = graph.paStructures.keys.map(_.begin).toSet
    val tags = tokens.indices.map { index =>
      if(predIndices.contains(index)) "V" else "O"
    }
    tokens.mkString(" ") + " ||| " + tags.mkString(" ")
  }

  def renderArgTaggedSeqs[Label](tokens: Vector[String], graph: QAMRGraph[Label]): List[String] = {
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

class GraphInducer(
  val structureInduction: StructureInduction,
  tokens: Vector[String], qas: List[SourcedQA[SentenceId]])(
  implicit inflections: Inflections,
  isStopword: IsStopword) {

  import structureInduction._

  val allContiguousSpans = qas.flatMap { sqa =>
    val qSpan = getWordsInQuestion(tokens, Tokenizer.tokenize(sqa.question));
    (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
  }

  val minimalContiguousSpans = getMinimalContiguousSpans(tokens, qas)

  val minimalSpanQuestionAppearanceCounts = Scorer[Set[Int], Int](
    qas.flatMap { qa =>
      val qSpan = getWordsInQuestion(tokens, Tokenizer.tokenize(qa.question))
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
      span => span.forall(getWordsInQuestion(tokens, Tokenizer.tokenize(qa.question)).contains)
    }.nextOption
  }.collect {
    case (Some(pred), qas) => pred -> qas
  }

  val edges = for {
    predSpan <- spansByPredicateness
    qa <- qasByPred.get(predSpan).getOrElse(Nil)
    qTokens = getWordsInQuestion(tokens, Tokenizer.tokenize(qa.question))
    answer <- qa.answers
    argSpan <- spansByPredicateness.find(_.forall(answer.contains)) // only attach to most predicatey answer
    if spanPredicateness(predSpan) > spanPredicateness(argSpan)
  } yield Edge(Some(qa.question), QAMRNode(predSpan.min, predSpan.max), QAMRNode(argSpan.min, argSpan.max))

  // agg edges together and ignore non-projective ones

  val paStructures = edges.foldLeft(Map.empty[QAMRNode, QAMRPAS[Set[String]]]) {
    case (acc, e @ Edge(qOpt, source, target)) =>
      if(isNewEdgeProjective(acc, e)) {
        acc.updated(
          source,
          acc.get(source) match {
            case None => QAMRPAS(source, Map(target -> qOpt.toSet))
            case Some(pas) => pas.addArg(target, qOpt.toSet)
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
        qTokens = getWordsInQuestion(tokens, Tokenizer.tokenize(qa.question))
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
