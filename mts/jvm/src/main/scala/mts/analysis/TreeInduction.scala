package mts.analysis

import mts.conll._
import mts.language._
import mts.util._
import scala.collection.mutable

object TreeInduction {

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

  sealed trait Index
  case object Root extends Index
  case class Word(word: CoNLLWord) extends Index

  @memoize(maxSize = 10, expiresAfter = 1 hour)
  def getAllIndices(sentence: CoNLLSentence): List[Index] = Root :: sentence.words.map(Word(_))

  def incoming(sentence: CoNLLSentence, index: Index): List[Arc] = getAllIndices(sentence).map(Arc(_, index))
  def outgoing(sentence: CoNLLSentence, index: Index): List[Arc] = getAllIndices(sentence).map(Arc(index, _))

  case class Arc(parent: Index, child: Index) {
    private[this] def shortIndex(i: Index) = i match {
      case Root => "$"
      case Word(w) => w.index
    }
    // not sure if variable names have to be unique...?
    override def toString: String = s"${shortIndex(parent)}-${shortIndex(child)}"
  }

  sealed trait LPVar
  case class ArcVar(arc: Arc) extends LPVar {
    override def toString = s"a$arc"
  }
  case class FlowVar(arc: Arc) extends LPVar {
    override def toString = s"f$arc"
  }
  case class DisjunctiveVar(arcs: List[Arc]) extends LPVar {
    override def toString = s"d${arcs.mkString(",")}".hashCode.toString.take(255)
  }

  import gurobi._

  def addArcVars(model: GRBModel, sentence: CoNLLSentence, vars: mutable.Map[LPVar, GRBVar]): Unit = {
    for {
      parent <- getAllIndices(sentence)
      child <- getAllIndices(sentence)
    } yield {
      val arc = Arc(parent, child)
      val arcVar = ArcVar(arc)
      // arc is binary --- TODO make this real valued if we do relaxation
      val arcGRBVar = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, arcVar.toString)
      vars.put(arcVar, arcGRBVar)
    }
    model.update()
  }

  def addFlowVars(model: GRBModel, sentence: CoNLLSentence, vars: mutable.Map[LPVar, GRBVar]): Unit = {
    for {
      parent <- getAllIndices(sentence)
      child <- getAllIndices(sentence)
    } yield {
      val arc = Arc(parent, child)
      val flowVar = FlowVar(arc)
      // flow ranges from 0 to n
      val flowGRBVar = model.addVar(0.0, sentence.words.size.toDouble, 0.0, GRB.CONTINUOUS, flowVar.toString)
      vars.put(flowVar, flowGRBVar)
    }
    model.update()
  }

  // assumes arc vars and flow vars present
  def constrainToArborescencePolytope(model: GRBModel, sentence: CoNLLSentence, vars: mutable.Map[LPVar, GRBVar]): Unit = {
    // CONSTRAINT SET 1 (n): each node except root has exactly one parent
    for {
      child <- getAllIndices(sentence)
      if child != Root
    } yield {
      val expr = new GRBLinExpr()
      for(arc <- incoming(sentence, child)) {
        expr.addTerm(1.0, vars(ArcVar(arc)))
      }
      model.addConstr(expr, GRB.EQUAL, 1.0, s"$child-parents")
    }

    // CONSTRAINT SET 2 (1): root has no parents
    val _ = {
      val expr = new GRBLinExpr()
      for(arc <- incoming(sentence, Root)) {
        expr.addTerm(1.0, vars(ArcVar(arc)))
      }
      model.addConstr(expr, GRB.EQUAL, 0.0, "root-noparents")
    }

    // CONSTRAINT SET 3 (1): root sends flow n
    val __ = {
      val expr = new GRBLinExpr()
      for(arc <- outgoing(sentence, Root)) {
        expr.addTerm(1.0, vars(FlowVar(arc)))
      }
      model.addConstr(expr, GRB.EQUAL, sentence.words.size.toDouble, "root-flow-n")
    }

    // CONSTRAINT SET 4 (n): Each non-root node consumes 1 flow
    for {
      node <- getAllIndices(sentence)
      if node != Root
    } yield {
      val expr = new GRBLinExpr()
      // add inward flow
      for(arc <- incoming(sentence, node)) {
        expr.addTerm(1.0, vars(FlowVar(arc)))
      }
      // subtract outward flow
      for(arc <- outgoing(sentence, node)) {
        expr.addTerm(-1.0, vars(FlowVar(arc)))
      }
      // result == 1
      model.addConstr(expr, GRB.EQUAL, 1.0, s"$node-flow-consume")
    }

    // CONSTRAINT SET 5 (n^2): flow is 0 on disabled arcs
    for {
      parent <- getAllIndices(sentence)
      child <- getAllIndices(sentence)
    } yield {
      val arc = Arc(parent, child)

      val left = new GRBLinExpr()
      left.addTerm(1.0, vars(FlowVar(arc)))
      val right = new GRBLinExpr()
      right.addTerm(sentence.words.size.toDouble, vars(ArcVar(arc)))
      model.addConstr(left, GRB.LESS_EQUAL, right, s"$parent-$child-flow")
    }
  }

  def induceTree(sentence: CoNLLSentence, qaPairs: List[(Set[Int], Set[Int])]): DependencyTree[Index, Unit] = {
    val env: GRBEnv = new GRBEnv("ilp-deps.log")
    val model: GRBModel = new GRBModel(env)
    model.set(GRB.IntParam.LogToConsole, 0)

    try {
      // Create variables
      val vars = mutable.Map.empty[LPVar, GRBVar]
      addArcVars(model, sentence, vars)
      addFlowVars(model, sentence, vars)
      constrainToArborescencePolytope(model, sentence, vars)

      // Add constraints

      // done with constraints; we're now in the arborescence polytope.
      // QA pairs determine the objective.
      val qaPairIndices = qaPairs.map {
        case (questionIndices, answerIndices) =>
          (questionIndices.map(sentence.words(_)), answerIndices.map(sentence.words(_)))
      }

      val objectiveScores = Scorer[LPVar, Double]

      // TODO factor the rest out into separate scoring methods

      val arcScores = Scorer[Arc, Double]
      for((qis, ais) <- qaPairIndices) {
        // ask for question words to be connected
        for(p <- qis; c <- qis) {
          if(p != c) arcScores.add(Arc(Word(p), Word(c)), 1.0)
        }
        // ask for answer words to be connected
        for(p <- ais; c <- ais) {
          if(p != c) arcScores.add(Arc(Word(p), Word(c)), 1.0)
        }

        // ask for question words to be connected to answer words (weighted down)
        val qToAScore = 1.0 / (qis.size * ais.size)
        // val aToQScore = 0.2 / (qis.size * ais.size)
        for(q <- qis; a <- ais) {
          if(q != a) {
            arcScores.add(Arc(Word(q), Word(a)), qToAScore)
            // arcScores.add(Arc(Word(a), Word(q)), aToQScore)
          }
        }
      }

      val objective = new GRBLinExpr()

      // // ask for question words to be connected to answer words (disjunctive)
      // val qaPairConnectionWeight = 0.5
      // for((qis, ais) <- qaPairIndices) {
      //   val qaArcs = (for(p <- qis; c <- ais) yield Arc(Word(p), Word(c))).toList
      //   val disjVar = DisjunctiveVar(qaArcs)
      //   val disjGRBVar = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, disjVar.toString)
      //   vars.put(disjVar, disjGRBVar)
      //   for(arc <- qaArcs) {
      //     val left = new GRBLinExpr()
      //     left.addTerm(1.0, disjGRBVar)
      //     val right = new GRBLinExpr()
      //     right.addTerm(1.0, vars(ArcVar(arc)))
      //     model.addConstr(left, GRB.GREATER_EQUAL, right, s"$disjVar-greater-$arc")
      //   }
      //   val left = new GRBLinExpr()
      //   left.addTerm(1.0, disjGRBVar)
      //   val right = new GRBLinExpr()
      //   for(arc <- qaArcs) {
      //     right.addTerm(1.0, vars(ArcVar(arc)))
      //   }
      //   model.addConstr(left, GRB.LESS_EQUAL, right, s"$disjVar-less")
      //   objective.addTerm(qaPairConnectionWeight, disjGRBVar)
      //   model.update()
      // }

      for((arc, weight) <- arcScores.iterator) {
        objective.addTerm(weight, vars(ArcVar(arc)))
      }
      model.setObjective(objective, GRB.MAXIMIZE)

      // run the optimization
      model.optimize()

      val arcs = for {
        pIndex <- getAllIndices(sentence)
        cIndex <- getAllIndices(sentence)
        arc = Arc(pIndex, cIndex)
        if vars(ArcVar(arc)).get(GRB.DoubleAttr.X) > 0.9 // TODO proper way to get binary value?
      } yield arc

      // since there are no cycles, we can safely do this
      def treeAt(index: Index): DependencyTree[Index, Unit] = {
        val childArcs = arcs
          .filter(_.parent == index)
          .map(arc => ((), treeAt(arc.child)))
        DependencyTree[Index, Unit](index, childArcs)
      }

      val tree = treeAt(Root)

      tree
    } catch {
      case e: Exception => e.printStackTrace
        null // TODO meh option return type
    } finally {
      model.dispose()
      env.dispose()
    }
  }
}
