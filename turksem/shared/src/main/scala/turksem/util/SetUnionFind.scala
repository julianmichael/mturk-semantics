package turksem.util

import scala.collection.immutable.SortedSet

// basic, inefficient immutable union-find based on sets
class SetUnionFind[A] private (
  val sets: Set[SortedSet[A]])(
  implicit ord: Ordering[A]) {

  def add(a: A): SetUnionFind[A] =
    if(sets.forall(!_.contains(a))) new SetUnionFind(sets + SortedSet[A](a))
    else this

  def find(a: A) = for {
    subset <- sets.find(_.contains(a))
  } yield subset.head

  def union(a: A, b: A): Option[SetUnionFind[A]] = for {
    aSet <- sets.find(_.contains(a))
    bSet <- sets.find(_.contains(b))
    } yield if(aSet != bSet) { // TODO should be by reference for efficiency
      new SetUnionFind(sets - aSet - bSet + (aSet ++ bSet))
    } else this

  def representatives: Set[A] = sets.map(_.head)
}
object SetUnionFind {
  def empty[A](implicit ord: Ordering[A]): SetUnionFind[A] =
    new SetUnionFind(Set[SortedSet[A]]())
}
