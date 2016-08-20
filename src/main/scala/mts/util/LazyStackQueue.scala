package mts.util

import scala.collection.mutable

/**
  * LazyStackQueue is a mutable data structure based on the following operations:
  *   push: adds to the top
  *   pop: takes from the top
  *   enqueue: adds to the bottom
  * The other main feature is that you instantiate it with an Iterator of starting items,
  * which it only traverses as necessary, buffering up to maxBufferSize elements at a time.
  * Note that items enqueued into the LazyStackQueue will only be popped off after the source iterator is empty.
  * LazyStackQueue should be viewed as "consuming" the iterator---you should not
  * use the iterator again after passing it into the constructor.
  */
class LazyStackQueue[A](
  private[this] val source: Iterator[A],
  private[this] val maxBufferSize: Int = 10
) {

  private[this] val top: mutable.Stack[A] = mutable.Stack.empty[A]

  // the middle acts as a buffer for the source iterator
  private[this] val middle: mutable.Queue[A] = mutable.Queue.empty[A]

  private[this] val bottom: mutable.Queue[A] = mutable.Queue.empty[A]

  def push(a: A): Unit = top.push(a)

  def enqueue(a: A): Unit = bottom.enqueue(a)

  def pop: Option[A] =
    top.popOption.orElse(popFromMiddleOption).orElse(bottom.dequeueOption)

  // filterPop returns the first element that satisfies the predicate, popping everything off on the way
  def filterPop(predicate: A => Boolean): Option[A] =
    Stream.continually(pop).dropWhile(!_.forall(predicate)).head

  def pop(n: Int): List[A] =
    Vector.fill(n)(pop).flatten.toList

  def filterPop(predicate: A => Boolean, n: Int): List[A] =
    Vector.fill(n)(filterPop(predicate)).flatten.toList

  private[this] def popFromMiddleOption: Option[A] = if(!middle.isEmpty) {
    Some(middle.dequeue)
  } else if(source.hasNext) {
    val result = source.next
    middle.enqueue(source.take(maxBufferSize).toSeq:_*)
    Some(result)
  } else {
    None
  }
}
