package mts.util

case class DependencyTree[Node, EdgeLabel](
  node: Node,
  children: List[(EdgeLabel, DependencyTree[Node, EdgeLabel])]) {

  def toStringMultiline(writeLabel: EdgeLabel => String, writeNode: Node => String): String =
    toStringMultilineAux(writeLabel, writeNode, 0)
  private def toStringMultilineAux(writeLabel: EdgeLabel => String, writeNode: Node => String, i: Int): String = {
    val indent = " " * i
    if(children.isEmpty) {
      s"$indent${writeNode(node)}"
    } else {
      val childrenString = children.map {
        case (label, tree) =>
          val treeString = tree.toStringMultilineAux(writeLabel, writeNode, i + 4)
          s"$indent  ${writeLabel(label)} -->\n$treeString"
      }.mkString("\n")
      s"$indent${writeNode(node)}\n$childrenString"
    }
  }

  // f(node, children.map(second.modify(_.fold(f))))
  def fold[A](f: (Node, List[(EdgeLabel, A)]) => A): A =
    f(node, children.map { case (l, c) => (l, c.fold(f)) })

  // fold((n: Node, c: List[(EdgeLabel, DependencyTree[A, EdgeLabel])]) => DependencyTree(f(n), c))
  // TODO can be more concise with Iso
  // maybe can remove type annotation with it too...
  def mapNodes[A](f: Node => A): DependencyTree[A, EdgeLabel] =
    DependencyTree(f(node), children.map { case (l, c) => (l, c.mapNodes(f)) })

  // fold(p => DependencyTree(p._1, p._2.map(first.modify(f))))
  def mapLabels[A](f: EdgeLabel => A): DependencyTree[Node, A] =
    DependencyTree(node, children.map { case (l, c) => (f(l), c.mapLabels(f))})

  def depth: Int = if(children.isEmpty) 0 else 1 + children.map(_._2.depth).max
  def size: Int = 1 + children.map(_._2.size).sum
}
