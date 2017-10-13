package turksem.util

import scala.collection.mutable

// Tarjan's efficient union-find data structure
class MutableUnionFind[A] {
  import MutableUnionFind.Cell
  private val cells: mutable.Map[A, Cell[A]] = mutable.Map.empty[A, Cell[A]]

  def add(a: A): Boolean = {
    if(!cells.contains(a)) {
      cells(a) = new Cell(a)
      true
    } else false
  }

  private[this] def findCell(a: A): Option[Cell[A]] = {
    cells.get(a).map { initCell =>
      val res = {
        var r = initCell
        while(r.parent != r) {
          r = r.parent
        }
        r
      }
      // path compression
      var replay = initCell
      while(replay.parent != replay) {
        val prev = replay
        replay = replay.parent
        prev.parent = res
      }
      res
    }
  }

  def find(a: A): Option[A] = findCell(a).map(_.item)

  def union(a: A, b: A): Boolean = {
    val resOpt = for {
      aRep <- findCell(a)
      bRep <- findCell(b)
    } yield {
      if(aRep == bRep) true
      else {
        // union by rank
        if(aRep.rank < bRep.rank) {
          aRep.parent = bRep
        } else if(bRep.rank < aRep.rank) {
          bRep.parent = aRep
        } else {
          aRep.parent = bRep
          bRep.rank = bRep.rank + 1
        }
        true
      }
    }
    resOpt.getOrElse(false)
  }

  def iterator = cells.keys.iterator
}
object MutableUnionFind {

  private class Cell[A](val item: A) {
    var parent: Cell[A] = this
    var rank: Int = 0
  }

  def empty[A]: MutableUnionFind[A] = new MutableUnionFind[A]()
  def apply[A]: MutableUnionFind[A] = new MutableUnionFind[A]()
}
