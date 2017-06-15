package qamr.util

import scala.collection.mutable

/** A queue with key-indexed access and removal.
  * Skimping on documentation here, fairly straightforward.
  */
class QueueMap[K, V] private () {
  import QueueMap._

  private[this] val map: mutable.Map[K, Node[K, V]] = mutable.Map.empty[K, Node[K, V]]
  private[this] val queue: Head[K, V] = Head[K, V]()

  def contains(k: K): Boolean = map.contains(k)
  def size: Int = map.size
  def isEmpty: Boolean = map.isEmpty

  /** Removes and returns the target element; returns None if not present. */
  def remove(k: K): Option[V] = map.get(k).map {
    case Node(key, value, prev, next) =>
      prev.next = next
      next.prev = prev
      map.remove(k)
      value
  }

  /** Removes and returns the first element in the queue; returns None if empty. */
  def pop: Option[V] = queue.next match {
    case Head() => None
    case Node(key, value, _, _) =>
      remove(key)
      Some(value)
  }

  /** Adds an element to the back of the queue. */
  def add(k: K, v: V): Unit = {
    map.remove(k)
    val node = Node(k, v, queue.prev, queue)
    map.put(k, node)
    queue.prev.next = node
    queue.prev = node
  }

  /** Iterates in pop-order over the elements of the queue. */
  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    var cur = queue.next
    override def hasNext: Boolean = cur match {
      case Head() => false
      case Node(_, _, _, _) => true
    }
    override def next: (K, V) = cur match {
      case Head() => throw new java.util.NoSuchElementException()
      case Node(k, v, _, nxt) =>
        cur = nxt
        (k, v)
    }
  }
}

/** Holds data definitions and factory methods for QueueMap. */
object QueueMap {
  def apply[K, V]() = new QueueMap[K, V]()
  def empty[K, V] = new QueueMap[K, V]()

  private sealed trait QueueElement[K, V] {
    var prev: QueueElement[K, V]
    var next: QueueElement[K, V]
  }
  private case class Node[K, V](
    key: K,
    value: V,
    override var prev: QueueElement[K, V],
    override var next: QueueElement[K, V]
  ) extends QueueElement[K, V]
  private case class Head[K, V]() extends QueueElement[K, V] {
    override var prev: QueueElement[K, V] = this
    override var next: QueueElement[K, V] = this
  }

}
