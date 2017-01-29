package mts.conll

import mts.util._

/** Represents a syntax tree from the CoNLL data.
  *
  * Intended to be used as an ADT, i.e.,
  * mostly what you will do with a SyntaxTree looks as follows.
  * {{{
  * val tree: SyntaxTree = // ...
  * tree match {
  * case SyntaxTreeNode(label, children) => // ...
  * case SyntaxTreeLeaf(word) => // ...
  * }
  * }}}
  */
sealed trait SyntaxTree {
  final def toStringMultiline: String = toStringMultilineAux(0)
  // TODO can have a better qualifier. why doesn't unqualified protected work?
  protected[conll] def toStringMultilineAux(indent: Int): String

  final def fold[A](leaf: CoNLLWord => A)(node: (String, List[A]) => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(label, children.map(_.fold(leaf)(node)))
  }
  // assume nonempty children
  final def depth = fold(_ => 0) { case (_, depths) => depths.max + 1}
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
  word: CoNLLWord
) extends SyntaxTree {
  override def toStringMultilineAux(i: Int): String = {
    val indent = " " * i
    val wordStr = s"${word.pos}\t${word.token}"
    s"$indent$wordStr"
  }

}

/** Provides parsing of SyntaxTrees. */
object SyntaxTree {

  // TODO import more specifically
  import scalaz._
  import Scalaz._
  private[this] type SentenceState[A] = State[List[CoNLLWord], A]

  import fastparse.all._
  private[this] val symbolP: P[String] = P(CharIn('A' to 'Z').rep.!)
  private[this] lazy val treeP: P[SentenceState[SyntaxTree]] =
    P("(" ~ symbolP ~ treeP.rep ~ ")").map {
      case (symbol, childrenState) => for {
        children <- childrenState.toList.sequence
      } yield SyntaxTreeNode(symbol, children.toList): SyntaxTree
    } | P("*").map { _ =>
      for {
        words <- State.get
        _ <- State.put(words.tail)
      } yield SyntaxTreeLeaf(words.head)
    }

  /** Parses a SyntaxTree from its flattened column representation in the CoNLL data.
    *
    * Assumes the data is in the correct format. Undefined behavior otherwise.
    *
    * @param s the flattened column representation of the tree
    * @param words the words of the sentence this tree parses
    */
  def fromString(s: String, words: List[CoNLLWord]): SyntaxTree =
    treeP.parse(s).get.value.eval(words)
}
