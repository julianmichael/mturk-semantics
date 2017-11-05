package turksem.util

import cats.kernel.CommutativeMonoid
import cats.kernel.CommutativeSemigroup

import scala.collection.mutable

// Tarjan's efficient union-find data structure
// ...tagged with auxiliary data for each set!
class MutableUnionFindTagged[A, B] {
  import MutableUnionFindTagged.Cell
  private val cells: mutable.Map[A, Cell[A, B]] = mutable.Map.empty[A, Cell[A, B]]

  def add(a: A)(implicit B: CommutativeMonoid[B]): Boolean = {
    if(!cells.contains(a)) {
      cells(a) = new Cell(a, B.empty)
      true
    } else false
  }

  // combines tag into the data even if a is already present
  def add(a: A, b: B)(implicit B: CommutativeSemigroup[B]): Unit = {
    cells.get(a) match {
      case None => cells(a) = new Cell(a, b)
      case Some(c) => c.tag = B.combine(c.tag, b)
    }
  }

  private[this] def findCell(a: A): Option[Cell[A, B]] = {
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

  def find(a: A): Option[(A, B)] = findCell(a).map(c => (c.item, c.tag))
  def findValue(a: A): Option[A] = findCell(a).map(_.item)
  def findTag(a: A): Option[B] = findCell(a).map(_.tag)

  def union(a1: A, a2: A)(implicit B: CommutativeSemigroup[B]): Boolean = {
    val resOpt = for {
      rep1 <- findCell(a1)
      rep2 <- findCell(a2)
    } yield {
      if(rep1 == rep2) true
      else {
        // union by rank
        if(rep1.rank < rep2.rank) {
          rep1.parent = rep2
          rep2.tag = B.combine(rep1.tag, rep2.tag)
        } else if(rep2.rank < rep1.rank) {
          rep2.parent = rep1
          rep1.tag = B.combine(rep1.tag, rep2.tag)
        } else {
          rep1.parent = rep2
          rep2.tag = B.combine(rep1.tag, rep2.tag)
          rep2.rank = rep2.rank + 1
        }
        true
      }
    }
    resOpt.getOrElse(false)
  }

  def iterator = cells.keys.iterator
}
object MutableUnionFindTagged {

  private class Cell[A, B](val item: A, var tag: B) {
    var parent: Cell[A, B] = this
    var rank: Int = 0
  }

  def empty[A, B]: MutableUnionFindTagged[A, B] = new MutableUnionFindTagged[A, B]()
  def apply[A, B]: MutableUnionFindTagged[A, B] = new MutableUnionFindTagged[A, B]()
}
