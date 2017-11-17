package example.emnlp2017

import cats.Eval
import cats.Foldable
import cats.Functor
import cats.Show
import cats.implicits._

import nlpdata.structure.Word
import nlpdata.util.LowerCaseStrings._

import turksem.util._

case class LemmaPosWord(
  index: Int,
  token: String,
  pos: String,
  lemma: LowerCaseString)

case class QAMRDependency[Label](
  label: Label,
  target: Int
)
object QAMRDependency {
  implicit val qamrDependencyFunctor: Functor[QAMRDependency] = new Functor[QAMRDependency] {
    override def map[A, B](fa: QAMRDependency[A])(f: A => B) =
      fa.copy(label = f(fa.label))
  }
}
case class QAMRDependencyGraph[A](
  // tops: List[Int], // TODO
  egresses: Map[Int, List[QAMRDependency[A]]]
) {

  def mapIndexedLabelsWithInversion[B](f: (A, Int, Int) => (B, Boolean)): QAMRDependencyGraph[B] = {
    QAMRDependencyGraph(
      egresses.flatMap { case (source, deps) =>
        deps.map { case QAMRDependency(label, target) =>
          val (newLabel, flip) = f(label, source, target)
          if(flip) source -> QAMRDependency(newLabel, target)
          else target -> QAMRDependency(newLabel, source)
        }
      }.groupBy(_._1).map { case (source, depPairs) => source -> depPairs.map(_._2).toList }
    )
  }

  def filter(p: A => Boolean): QAMRDependencyGraph[A] = {
    QAMRDependencyGraph(
      egresses.map { case (source, deps) =>
        source -> deps.filter(dep => p(dep.label))
      }
    )
  }

  def roots = egresses.iterator.collect {
    case (source, arcs) if !egresses.values.flatten.exists(_.target == source) => source
  }.toList.sorted

  def predicates = egresses.iterator.collect {
    case (source, arcs) if arcs.nonEmpty => source
  }.toList.sorted

  private def hasCycleAux(node: Int, ancestors: Set[Int]): Boolean = {
    ancestors.contains(node) || egresses.get(node).nonEmptyAnd(arcs =>
      arcs.map(_.target).exists(hasCycleAux(_, ancestors + node))
    )
  }
  def hasCycle: Boolean = roots.exists(hasCycleAux(_, Set.empty[Int]))

}
object QAMRDependencyGraph {
  implicit val qamrDependencyGraphFunctor: Functor[QAMRDependencyGraph] = new Functor[QAMRDependencyGraph] {
    override def map[A, B](fa: QAMRDependencyGraph[A])(f: A => B) =
      QAMRDependencyGraph(
        fa.egresses.map { case (k, v) => k -> v.map(_.map(f)) }
      )
  }

  implicit val qamrDependencyGraphFoldable: Foldable[QAMRDependencyGraph] = new Foldable[QAMRDependencyGraph] {
    override def foldLeft[A, B](fa: QAMRDependencyGraph[A], b: B)(f: (B, A) => B): B = {
      fa.egresses.iterator.flatMap(_._2).map(_.label).foldLeft(b)(f)
    }
    override def foldRight[A, B](fa: QAMRDependencyGraph[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      implicitly[Foldable[List]].foldRight(fa.egresses.iterator.flatMap(_._2).map(_.label).toList, b)(f)
    }
  }

  def readFromSemEvalFormattedLines[Label](
    lines: List[String],
    readLabel: String => Label
  ): (QAMRDependencyGraph[Label], Vector[LemmaPosWord]) = {
    case class LineRepr(word: LemmaPosWord, isTop: Boolean, isPred: Boolean, ingressByPredNumber: Map[Int, QAMRDependency[Label]])
    val reprs = lines.zipWithIndex.map { case (line, index) =>
      (line.split("\t").toList: @unchecked) match {
        case _ :: token :: lemma :: pos :: top :: pred :: sense :: ingress =>
          val word = LemmaPosWord(
            index = index,
            token = token,
            pos = pos,
            lemma = lemma.lowerCase)
          val isTop = top == "+"
          val isPred = pred == "+"
          val arcs = ingress.zipWithIndex.filter(_._1 != "_").map {
            case (labelStr, predNumber) => predNumber -> QAMRDependency(readLabel(labelStr), index)
          }.toMap
          LineRepr(word, isTop, isPred, arcs)
      }
    }.toVector
    val words = reprs.map(_.word)
    val predIndices = reprs.filter(_.isPred).map(_.word.index)
    val egresses = predIndices.zipWithIndex.map { case (predIndex, predNumber) =>
      predIndex -> reprs.flatMap(_.ingressByPredNumber.get(predNumber)).toList
    }.toMap
    (QAMRDependencyGraph(egresses), words)
  }

  def makeSemEvalFormattedString[Label : Show](
    words: Vector[LemmaPosWord],
    graph: QAMRDependencyGraph[Label],
    dummyAugmented: Boolean = true
  ): String = {
    val roots = graph.roots
    val preds = graph.predicates
    words.map { w =>
      List(
        w.index + 1,
        w.token,
        w.lemma.toString,
        w.pos
      ) ++ (
        if(dummyAugmented) List("*", "*") else Nil
      ) ++ List(
        if(roots.headOption.nonEmptyAnd(_ == w.index)) "+" else "-", // top (choose a root?)
        if(preds.contains(w.index)) "+" else "-", // pred (has outgoing edges)
        "_" // sense
      ) ++ preds.map(i =>
        graph.egresses(i).find(_.target == w.index).fold("_")(_.label.show)
      )
    }.map(_.mkString("\t") + "\n").mkString
  }
}
