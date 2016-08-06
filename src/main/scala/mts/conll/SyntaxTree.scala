package mts.conll

sealed trait SyntaxTree {
  final def toStringMultiline: String = toStringMultiline(0)
  def toStringMultiline(indent: Int): String
}
case class SyntaxTreeNode(
  label: String,
  children: List[SyntaxTree]
) extends SyntaxTree {
  override def toStringMultiline(i: Int): String =
    s"${" " * i}$label(\n${children.map(_.toStringMultiline(i + 1)).mkString("\n")})"
}
case class SyntaxTreeLeaf(
  word: CoNLLWord
) extends SyntaxTree {
  override def toStringMultiline(i: Int): String =
    s"${" " * i}${word.pos}\t${word.token}"
}

object SyntaxTree {
  // TODO import more specifically...
  import scalaz._
  import Scalaz._
  type SentenceState[A] = State[List[CoNLLWord], A]

  import fastparse.all._
  val symbolP: P[String] = P(CharIn('A' to 'Z').rep.!)
  lazy val treeP: P[SentenceState[SyntaxTree]] =
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

  def fromString(s: String, words: List[CoNLLWord]): Option[SyntaxTree] =
    scala.util.Try(treeP.parse(s).get.value.eval(words)).toOption
}
