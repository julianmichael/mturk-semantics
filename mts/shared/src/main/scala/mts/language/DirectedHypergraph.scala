package mts.language

case class Hyperedge[Node, EdgeLabel](
  label: EdgeLabel,
  target: Node,
  source: Set[Node])

// all source/target nodes in the edges must be in the "nodes" set
case class DirectedHypergraph[Node, EdgeLabel](
  nodes: Set[Node],
  edges: Set[Hyperedge[Node, EdgeLabel]]) {

  def filterEdges(p: Hyperedge[Node, EdgeLabel] => Boolean) =
    DirectedHypergraph(nodes, edges.filter(p))

  def mapNodes[A](f: Node => A): DirectedHypergraph[A, EdgeLabel] =
    DirectedHypergraph(
      nodes.map(f), edges.map {
        case Hyperedge(label, target, source) =>
          Hyperedge(label, f(target), source.map(f))
      })

  def mapLabels[A](f: EdgeLabel => A): DirectedHypergraph[Node, A] =
    DirectedHypergraph(
      nodes, edges.map {
        case Hyperedge(label, target, source) =>
          Hyperedge(f(label), target, source)
      })


  // def filterNodes(p: Node => Boolean) =
  //   DirectedHypergraph(nodes.filter(p), edges.filter(e => (e.source + e.target).forall(p)))
}
