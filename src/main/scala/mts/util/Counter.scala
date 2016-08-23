package mts.util

import scala.collection.mutable

/**
  * Convenience class for counting things.
  * Could definitely replace with a better implementation later on...
  */
class Counter[A] private (private[this] val map: mutable.Map[A, Int]) {
  def get(a: A): Int = map.get(a).getOrElse(0)
  def add(a: A): Unit = map.put(a, get(a) + 1)
  def iterator: Iterator[(A, Int)] = map.iterator
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
