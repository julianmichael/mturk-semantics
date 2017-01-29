package mts.language

// haw haw.
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

  // def fold[B](f: (Node, List[(EdgeLabel, B)]) => B): B =
  //   f(node, children.map { case (l, child) => (l, child.fold(f)) })

  def depth: Int = if(children.isEmpty) 0 else 1 + children.map(_._2.depth).max
  def size: Int = 1 + children.map(_._2.size).sum
}
