package mts.datasets.ptb

import mts.util._

// TODO import specifically
import scalaz._
import Scalaz._

/** Represents a syntax tree from the PTB data.
  * TODO: children should be nonempty list
  */
sealed trait SyntaxTree {
  final def fold[A](leaf: Word => A)(node: (String, List[A]) => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(label, children.map(_.fold(leaf)(node)))
  }

  final def foldUnlabeled[A](leaf: Word => A)(node: List[A] => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(children.map(_.foldUnlabeled(leaf)(node)))
  }

  // final def foldM[M: Monad](leaf: Word => M[A])(node: (String, List[A]) => M[A]): M[A] = this match {
  //   case SyntaxTreeLeaf(word) => leaf(word)
  //   case SyntaxTreeNode(label, children) =>
  //     children.map(_.foldM(leaf)(node)).sequence.flatMap(node(label, _))
  // }

  // final def foldMUnlabeled[M: Monad](leaf: Word => M[A])(node: List[A] => M[A]): M[A] = this match {
  //   case SyntaxTreeLeaf(word) => leaf(word)
  //   case SyntaxTreeNode(label, children) =>
  //     children.map(_.foldM(leaf)(node)).sequence.flatMap(node)
  // }

  // which of the following is the better generalization? guess the first... kind of the same
  // first one lets you avoid intermediate structure, second lets you delay msumming to later....
  // but they can obv replicate each other trivially
  // guess the second one obviates the need to map to something right away...
  // final def wordsMonoid[M : Monoid](leaf: Word => M) = foldUnlabeled(leaf)(_.msum)
  // final def wordsList = fold(List(_))(_.flatten)
  final def words = foldUnlabeled(Vector(_))(_.toVector.flatten)

  final def depth = foldUnlabeled(_ => 0)(_.max + 1)
  final def beginIndex = foldUnlabeled(_.index)(_.min)
  final def endIndex = foldUnlabeled(_.index)(_.max)

  final def toStringMultiline: String = toStringMultilineAux(0)
  // TODO could do this with state monad lol
  protected[ptb] def toStringMultilineAux(indent: Int): String
}

/** Represents a nonterminal node of a SyntaxTree.
  *
  * @param label the nonterminal symbol of this node
  * @param this node's children
  */
case class SyntaxTreeNode(
  label: String,
  children: List[SyntaxTree]
) extends SyntaxTree {
  override def toStringMultilineAux(i: Int): String = {
    val indent = " " * i
    val childrenStr = children.map(_.toStringMultilineAux(i + 1)).mkString("\n")
    s"$indent$label\n$childrenStr"
  }
}

/** Represents a terminal node of a SyntaxTree, including POS tag.
  *
  * @param word the word at this node
  */
case class SyntaxTreeLeaf(
  word: Word
) extends SyntaxTree {
  override def toStringMultilineAux(i: Int): String = {
    val indent = " " * i
    val wordStr = s"${word.pos}\t${word.token}"
    s"$indent$wordStr"
  }

}
