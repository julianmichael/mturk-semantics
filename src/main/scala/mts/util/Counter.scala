package mts.util

import scala.collection.mutable

/** Convenience class for counting things. Mutable.
  *
  * Could definitely replace with a better implementation later on,
  * maybe just use something from a library.
  * Not writing scaladoc because it's pretty self-explanatory...
  */
class Counter[A] private (private[this] val map: mutable.Map[A, Int]) {
  def get(a: A): Int = map.get(a).getOrElse(0)
  def add(a: A): Unit = addN(a, 1)
  def addN(a: A, n: Int): Unit = map.put(a, get(a) + n)
  def size: Int = map.size

  def addAll(other: Iterator[(A, Int)]): Unit =
    other.foreach(Function.tupled(addN))

  def iterator: Iterator[(A, Int)] = map.iterator
  def keyIterator: Iterator[A] = map.keys.iterator

  def median: Double = {
    if(map.isEmpty) {
      0.0
    } else {
      val sorted = iterator.map(_._2).toVector.sorted
      if(sorted.size % 2 == 0) {
        (sorted(sorted.size / 2 - 1) + sorted(sorted.size / 2)).toDouble / 2.0
      } else {
        sorted(sorted.size / 2)
      }
    }
  }
  def max: Int = if(map.isEmpty) 0 else map.values.max
  def min: Int = if(map.isEmpty) 0 else map.values.min
  def sum: Int = map.values.sum
}

object Counter {
  def apply[A](): Counter[A] = {
    new Counter(mutable.Map.empty[A, Int])
  }

  def apply[A](items: TraversableOnce[A]): Counter[A] = {
    val map = mutable.Map.empty[A, Int]
    val counter = new Counter(map)
    items.foreach(i => counter.add(i))
    counter
  }
}
