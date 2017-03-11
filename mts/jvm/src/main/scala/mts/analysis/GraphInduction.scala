package mts.analysis

import mts.util._
import mts.language.Graph
import mts.language.Edge
import mts.datasets.conll._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

object GraphInduction {
  def edgesFor[Node : Ordering](nodes: Set[Node]) = {
    val withRepeats = for {
      i <- nodes
      j <- nodes
      if i != j
    } yield Edge(i, j)
    withRepeats.toSet
  }

  val stepSize = 1.0

  // expects a lot of negative weights and few zero weights
  def thresholdedMaxSpanningGraph[Node : Ordering, UnionFind[_]](
    nodes: Set[Node],
    getWeight: (Edge[Node] => Double),
    threshold: Double // expect this to be negative
  )(implicit UnionFind: OrderedUnionFind[UnionFind]
  ): (Graph[Node], Double) = {
    var components = UnionFind.empty[Node]
    var edges = Set.empty[Edge[Node]]
    // add nodes
    nodes.foreach(node => components = UnionFind.add[Node](components, node))

    def addEdge(e: Edge[Node]) = {
      edges = edges + e
      components = UnionFind.union[Node](components, e.lesser, e.greater).get
    }

    val weightedEdges = {
      val withRepeats = for {
        n1 <- nodes
        n2 <- nodes
        if n1 != n2
      } yield Edge(n1, n2)
      // sort by decreasing weight
      withRepeats
        .toSet.toVector
        .map((e: Edge[Node]) => (e, getWeight(e)))
    }

    val (positiveEdges, negativeEdges) = weightedEdges.partition(_._2 > 0.0)

    // add ALL positive-weight edges
    positiveEdges.map(_._1).foreach(addEdge)

    var numComponentsRemaining = UnionFind.representatives[Node](components).size

    val sortedNegativeEdges = negativeEdges
      .filter(_._2 > threshold) // ignore edges with weight below the threshold
      .sortBy(-_._2) // sort decreasing by weight for greedy algorithm

    // add edges in order of decreasing weight as long as they connect new components.
    for((Edge(lesser, greater), weight) <- sortedNegativeEdges) {
      if(numComponentsRemaining > 1) { // stop if the graph is now connected
        if(UnionFind.find[Node](components, lesser) != UnionFind.find[Node](components, greater)) { // only if it helps connect components
          addEdge(Edge(lesser, greater))
          numComponentsRemaining = numComponentsRemaining - 1
        }
      }
    }

    // this is our little submodel
    val score = edges.map(getWeight).sum + ((numComponentsRemaining - 1) * threshold)

    (Graph(nodes, edges), score)
  }

  // assume sets nonempty
  def chooseQAArc(qIndices: Set[Int], aIndices: Set[Int], getWeight: (Int, Int) => Double) = {
    val possiblePairs = {
      val possibleArcsIter = for {
        qi <- qIndices.iterator
        ai <- aIndices.iterator
        if qi != ai
      } yield (qi, ai)
      possibleArcsIter.toSet
    }

    val possibleArcs = possiblePairs
      .map(p => Edge(p._1, p._2))
    val scoredPossibleArcsList = possiblePairs.toList
      .map(p => Edge(p._1, p._2))
      .zip(possiblePairs.map(Function.tupled(getWeight(_, _))))
    val (positives, negatives) = scoredPossibleArcsList.partition(_._2 > 0.0)
    if(!positives.isEmpty) {
      (possibleArcs, positives.map(_._1).toSet)
    } else {
      val singleArc = scala.util.Try(negatives.maxBy(_._2)._1).toOption.toSet
      (possibleArcs, singleArc)
    }
  }

  def induceGraphSingleTreeSubroutines(
    connectedSets: Vector[Set[Int]], // answer spans
    qaArcSets: Vector[(Set[Int], Set[Int])]
  ): Graph[Int] = {
    val allIndices = connectedSets.flatten.toSet ++ qaArcSets.map(p => p._1 ++ p._2).flatten
    val allEdges = edgesFor(allIndices)
    val questionIndexCounts = Scorer[Int, Int](qaArcSets.map(_._1.toList).flatten)

    def initEdgeWeight(e: Edge[Int]) = -5.0 - ((e.greater - e.lesser) / 100.0)
    def qaArcWeight(qi: Int, ai: Int) = -5.0 + (0.1 * questionIndexCounts(ai))

    var u = Map.empty[Edge[Int], Double]
    var lambdas = Vector.fill(connectedSets.size + qaArcSets.size)(Map.empty[Edge[Int], Double].withDefaultValue(0.0))

    def newGraphs = connectedSets.zip(lambdas.take(connectedSets.size)).map {
      case (nodes, lams) =>
        thresholdedMaxSpanningGraph[
          Int,
          ({ type λ[A] = Set[SortedSet[A]]})#λ](
          nodes,
          (edge: Edge[Int]) => initEdgeWeight(edge) + lams(edge),
          -100.0)._1 match {
          case Graph(_, edges) =>
            (edgesFor(nodes), edges)
        }
    } ++ qaArcSets.zip(lambdas.drop(connectedSets.size)).map {
      case ((qis, ais), lams) => chooseQAArc(qis, ais, ((qi: Int, ai: Int) => qaArcWeight(qi, ai) + lams(Edge(qi, ai))))
    }

    var graphs: Vector[(Set[Edge[Int]], Set[Edge[Int]])] = newGraphs

    def update = {
      u = allEdges.iterator.map { e =>
        val agg = graphs.collect {
          case (possibleEdges, edges) if possibleEdges.contains(e) =>
            if(edges.contains(e)) 1.0 else 0.0
        }
        e -> (if(agg.isEmpty) 0.0 else agg.mean)
      }.toMap

      lambdas = graphs.zip(lambdas).map {
        case (graph, lams) => graph._1.map { edge =>
          val graphEdgeIndicator = if(graph._1.contains(edge)) 1.0 else 0.0
          val newLam = (lams(edge) - (stepSize * (graphEdgeIndicator - u(edge))))
          edge -> newLam
        }.toMap
      }

      graphs = newGraphs
    }

    var undecidedArcs = 0.0 // tmp
    var iter = 0
    do {
      iter = iter + 1
      update
      undecidedArcs = u.values.map(v => math.min(1.0 - v, v)).sum
      println(f"iterating... $undecidedArcs%.2f undecidedness; multipliers ${lambdas.map(_.values.map(x => x * x).sum).sum}%.2f")
    } while(undecidedArcs > 0.01 && iter < 100)

    // now u should have 1s and 0s only and repr. the final graph
    // or we hit 100 iterations and stopped.

    u.foreach {
      case (edge, ind) => println(s"$edge: $ind")
    }

    val finalEdges = u.collect {
      case (edge, ind) if ind > 0.5 => edge
    }.toSet

    Graph(allIndices, finalEdges)
  }

  // of course, the "relaxed" version is way harder to solve...
  def thresholdedMaxSpanningGraphRelaxed[Node, UnionFind[_]](
    nodes: Set[Node],
    getWeight: (Edge[Node] => Double),
    threshold: Double // expect this to be negative
  )(implicit ord: Ordering[Node],
    UnionFind: OrderedUnionFind[UnionFind]
  ): (Set[Node], Scorer[Edge[Node], Double]) = {
    var components = UnionFind.empty[Node]
    val edgeScores = Scorer[Edge[Node], Double]
      // Map.empty[Edge[Node], Double].withDefaultValue(0.0)
    // add nodes
    nodes.foreach(node => components = UnionFind.add[Node](components, node))

    // get all relevant edges/weights
    // sort by decreasing weight
    val weightedEdges = edgesFor(nodes).toVector
      .map((e: Edge[Node]) => (e, getWeight(e)))

    val (positiveEdges, negativeEdges) = weightedEdges.partition(_._2 > 0.0)

    // add ALL positive-weight edges
    positiveEdges.map(_._1).foreach { e =>
      edgeScores.add(e, 1.0) // full weight for each since it must be included
      components = UnionFind.union[Node](components, e.lesser, e.greater).get
    }

    // group negative-weight edges and sort by weight
    val sortedGroupedNegativeEdges = negativeEdges
      .filter(_._2 > threshold) // ignore edges with weight below the threshold
      .groupBy(_._2).toVector
      .sortBy(-_._1) // sort decreasing by weight for greedy algorithm
      .map {
      case (score, scoredEdges) => scoredEdges.map(_._1).toList
    }.toList

    def getRepresentativeEdge(
      components: UnionFind[Node],
      edge: Edge[Node]
    ) = Edge(
      UnionFind.find[Node](components, edge.lesser).get,
      UnionFind.find[Node](components, edge.greater).get)

    def getWeightedEdgeCounts(
      components: UnionFind[Node],
      edges: List[Edge[Node]],
      numComponents: Int,
      goalNumComponents: Int): (Map[Edge[Node], Int], Int) = {

      if(numComponents == goalNumComponents) {
        (Map.empty[Edge[Node], Int], 1) // 1 instance of tree
      } else {
        val edgesByRepresentative = edges
          .groupBy(e => getRepresentativeEdge(components, e))
          .filterNot(p => p._1.lesser == p._1.greater)
        val representativeEdges = edgesByRepresentative
          .keys.toList
        // must be nonempty
        representativeEdges match {
          case Nil => (Map.empty[Edge[Node], Int], 0) // no spanning tree possible --- 0 instances
          case next :: remainder =>
            val withEdgePair = {
              val preEdge = getWeightedEdgeCounts(
                UnionFind.union[Node](components, next.lesser, next.greater).get,
                remainder,
                numComponents - 1,
                goalNumComponents)
              // add the representative edge to the result
              (preEdge._1.updated(next, preEdge._2), preEdge._2)
            }
            val withoutEdgePair = getWeightedEdgeCounts(
              components,
              remainder,
              numComponents,
              goalNumComponents)

            // start with reps
            var allEdgeCounts = withoutEdgePair._1.foldLeft(withEdgePair._1) {
              case (acc, (edge, count)) => acc.updated(edge, acc.get(edge).getOrElse(0) + count)
            }
            var treeCount = withoutEdgePair._2 + withEdgePair._2

            // distribute counts between edges with same representative
            edgesByRepresentative.foreach {
              case (rep, edges) =>
                // multiply all counts by the number of edge choices
                allEdgeCounts = allEdgeCounts.map {
                  case (k, v) => k -> (v * edges.size)
                }
                treeCount = treeCount * edges.size
                // split the representative among its constituents
                val curRepCount = allEdgeCounts.get(rep).getOrElse(0)
                allEdgeCounts = edges.foldLeft(allEdgeCounts - rep) {
                  case (acc, e) => acc.updated(e, curRepCount / edges.size) // should be 0 currently
                }
            }
            (allEdgeCounts, treeCount)
        }
      }
    }

    def addEdges(es: List[Edge[Node]]) = {
      val numCurrentComponents = UnionFind.representatives(components).size
      val numGoalComponents = {
        val goalComps = es.foldLeft(components) {
          case (comps, e) => UnionFind.union(comps, e.lesser, e.greater).get
        }
        UnionFind.representatives(goalComps).size
      }

      // determine what relaxed counts to assign equal-weight edges
      val (weightedEdgeCounts, numArrangements) =
        getWeightedEdgeCounts(components, es, numCurrentComponents, numGoalComponents)

      val normalizedWeightedEdgeCounts = weightedEdgeCounts.map {
        case (k, v) => k -> (v.toDouble / numArrangements)
      }

      // update the global edge county thing
      normalizedWeightedEdgeCounts.foreach {
        case (e, c) => edgeScores.add(e, c)
      }
    }

    // add edges in order of decreasing weight as long as they connect new components.
    for(edgeGroup <- sortedGroupedNegativeEdges) {
      val numComponentsRemaining = UnionFind.representatives[Node](components).size
      if(numComponentsRemaining > 1) { // stop if the graph is now connected
          addEdges(edgeGroup)
      }
    }

    (nodes, edgeScores)
  }

  def induceGraphRelaxedSubroutine(connectedSets: Vector[Set[Int]]): Graph[Int] = {
    val allIndices = connectedSets.flatten.toSet
    val allEdges = edgesFor(allIndices)

    val edgeWeight = -1.0 // negative so we include as few edges as possible

    // u stands for the "average" towards which we regularize
    var u = Map.empty[Edge[Int], Double]
    // lambdas are the lagrange multipliers regulating the constraints between u and the graphs
    var lambdas = Vector.fill(connectedSets.size)(Map.empty[Edge[Int], Double].withDefaultValue(0.0))
    // these graphs are the solutions to all the subproblems; initialized to weigh every edge equally
    var graphs: Vector[(Set[Int], Scorer[Edge[Int], Double])] = connectedSets.map { nodes =>
      val scores = Scorer[Edge[Int], Double]
      edgesFor(nodes).foreach(e => scores.add(e, 2.0 / nodes.size))
      (nodes, scores)
    }
    // the above initialization weighs each edge equally as the probability it would appear in a spanning tree,
    // assuming the subgraph is complete and all edges are equally likely (the probability is 2/|V|).

    // for each edge, u(edge) is the average of the values for the edge where it is present in a subproblem.
    def newU = allEdges.iterator.map {
      case e @Edge(i, j) => e -> graphs.collect {
        case (nodes, edgeScores) if nodes.contains(i) && nodes.contains(j) =>
          edgeScores(e)
      }.mean
    }.toMap

    // lambda for a (subproblem, edge) is updated towards the difference between
    // the average for that edge and the value for that edge in the subproblem
    def newLambdas = graphs.zip(lambdas).map {
      case (graph, lams) =>
        edgesFor(graph._1).map { edge =>
          val newLam = (lams(edge) - (stepSize * (graph._2(edge) - u(edge))))
          edge -> newLam
        }.toMap
    }

    // re-solve for the new (relaxed) graphs with the current lambdas
    def newGraphs = connectedSets.zip(lambdas).map {
      case (nodes, lams) =>
        thresholdedMaxSpanningGraphRelaxed[Int, ({ type λ[A] = Set[SortedSet[A]]})#λ](
          nodes,
          (edge: Edge[Int]) => edgeWeight + lams(edge),
          -100.0)
    }

    def update = {
      u = newU
      lambdas = newLambdas
      graphs = newGraphs
    }

    var undecidedness = 0.0 // will be 0 again if all structures agree and are binary
    var iter = 0
    do {
      iter = iter + 1
      update
      undecidedness = u.values.map(v => math.min(1.0 - v, v)).sum
      val uDiff = newU.keys.map(e => math.pow(newU(e) - u(e), 2)).sum
      val multiplierSq = lambdas.map(_.values.map(x => x * x).sum).sum
      println(f"iterating... $undecidedness%.2f undecidedness; $uDiff%.2f disagreement; multipliers $multiplierSq%.2f")
    } while(undecidedness > 0.01 && iter < 100)

    // now u should have 1s and 0s only and represent the final graph

    u.foreach {
      case (edge, ind) => println(s"$edge: $ind")
    }

    val finalEdges = u.collect {
      case (edge, ind) if ind > 0.9 => edge
    }.toSet

    Graph(allIndices, finalEdges)
  }
}
