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
  final def toStringMultiline: String = toStringMultiline(0)
  protected def toStringMultiline(indent: Int): String
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
  protected override def toStringMultiline(i: Int): String =
    s"${" " * i}$label(\n${children.map(_.toStringMultiline(i + 1)).mkString("\n")})"
}

/** Represents a terminal node of a SyntaxTree, including POS tag.
  *
  * @param word the word at this node
  */
case class SyntaxTreeLeaf(
  word: CoNLLWord
) extends SyntaxTree {
  protected override def toStringMultiline(i: Int): String =
    s"${" " * i}${word.pos}\t${word.token}"
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
