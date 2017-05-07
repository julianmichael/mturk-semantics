package mts.language

// undirected
case class Graph[Node](
  nodes: Set[Node],
  edges: Set[Edge[Node]])

sealed trait Edge[Node] {
  def lesser: Node
  def greater: Node
}
object Edge {
  private[this] case class EdgeImpl[Node](override val lesser: Node, override val greater: Node) extends Edge[Node]
  def apply[Node](x: Node, y: Node)(implicit ord: Ordering[Node]): Edge[Node] = if(ord.lteq(x, y)) EdgeImpl(x, y) else EdgeImpl(y, x)
  def unapply[Node](e: Edge[Node]): Some[(Node, Node)] = Some((e.lesser, e.greater))
}
