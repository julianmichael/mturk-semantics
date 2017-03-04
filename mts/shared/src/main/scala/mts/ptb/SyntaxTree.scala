package mts.ptb

import mts.util._

case class Word(
  index: Int,
  pos: String,
  token: String)

/** Represents a syntax tree from the PTB data.
  */
sealed trait SyntaxTree {
  final def toStringMultiline: String = toStringMultilineAux(0)
  protected[ptb] def toStringMultilineAux(indent: Int): String

  final def fold[A](leaf: Word => A)(node: (String, List[A]) => A): A = this match {
    case SyntaxTreeLeaf(word) => leaf(word)
    case SyntaxTreeNode(label, children) => node(label, children.map(_.fold(leaf)(node)))
  }
  // assume nonempty children
  final def depth = fold(_ => 0) { case (_, depths) => depths.max + 1}
  // generalizes to monoids
  final def words = fold(Vector(_))((_, children) => children.toVector.flatten)
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

/** Provides parsing of SyntaxTrees for the PTB format. */
object SyntaxTree {

  // TODO import more specifically
  import scalaz._
  import Scalaz._
  private[this] type SentenceState[A] = State[Int, A]

  import fastparse.all._
  private[this] val symbolP: P[String] = P(CharPred(c =>  !" ()".contains(c)).rep.!)
     // P(CharIn('A' to 'Z', '0' to '9', "-$,.").rep.!)
  private[this] val tokenP: P[String] = P(CharPred(c =>  !" ()".contains(c)).rep.!)
  private[this] lazy val treeP: P[SentenceState[SyntaxTree]] =
    P("(" ~ symbolP ~ " " ~ treeP.rep ~ ")").map {
      case (symbol, childrenState) => for {
        children <- childrenState.toList.sequence
      } yield SyntaxTreeNode(symbol, children.toList): SyntaxTree
    } | P("(" ~ symbolP ~ " " ~ tokenP ~ ")" ~ " ".?).map {
      case (pos, token) => for {
        index <- State.get
        _ <- State.put(index + 1)
      } yield SyntaxTreeLeaf(Word(index, pos, token)): SyntaxTree
    }
  private[this] val fullTreeP: P[SyntaxTree] =
    P("(" ~ " ".? ~ treeP ~ ")").map(_.eval(0))

  /** Parses a SyntaxTree from its flattened column representation in the CoNLL data.
    *
    * Assumes the data is in the correct format. Undefined behavior otherwise.
    *
    * @param s the flattened column representation of the tree
    * @param words the words of the sentence this tree parses
    */
  def fromString(s: String): SyntaxTree =
    fullTreeP.parse(s).get.value
}
