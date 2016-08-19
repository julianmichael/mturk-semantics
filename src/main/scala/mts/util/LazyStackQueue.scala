package mts.util

import scala.collection.mutable

/**
  * LazyStackQueue is a mutable data structure based on the following operations:
  *   push: adds to the top
  *   pop: takes from the top
  *   enqueue: adds to the bottom
  * The other main feature is that you instantiate it with an Iterator of starting items,
  * which it only traverses as necessary, pulling groups of elements of size bufferSize at a time.
  * Items enqueued into the LazyStackQueue will only be popped off after the source iterator is empty.
  * LazyStackQueue should be viewed as "consuming" the iterator---you should not
  * use the iterator again after passing it into the constructor.
  */
class LazyStackQueue[A](
  private[this] val source: Iterator[A],
  private[this] val bufferSize: Int = 10
) {

  private[this] val preStack: mutable.Stack[A] = mutable.Stack.empty[A]

  // the middle: buffer for the source iterator
  private[this] val midQueue: mutable.Queue[A] = mutable.Queue.empty[A]

  private[this] val postQueue: mutable.Queue[A] = mutable.Queue.empty[A]

  def push(a: A): Unit = preStack.push(a)

  def enqueue(a: A): Unit = postQueue.enqueue(a)

  def pop: Option[A] = popFromTop.orElse(popFromMiddle).orElse(popFromBottom)
  // filterPop pops repeatedly until the popped item satisfies the predicate, and returns that item.
  def filterPop(predicate: A => Boolean): Option[A] = {
    var cur = pop
    while(!cur.forall(predicate)) {
      cur = pop
    }
    cur
  }
  def pop(n: Int): List[A] = Vector.fill(n)(pop).flatten.toList
  def filterPop(predicate: A => Boolean, n: Int): List[A] = Vector.fill(n)(filterPop(predicate)).flatten.toList

  private[this] def popFromTop: Option[A] = if(!preStack.isEmpty) Some(preStack.pop) else None
  private[this] def popFromMiddle: Option[A] = if(!midQueue.isEmpty) {
    Some(midQueue.dequeue)
  } else if(source.hasNext) {
    val result = source.next
    midQueue.enqueue(source.take(bufferSize).toSeq:_*)
    Some(result)
  } else {
    None
  }
  private[this] def popFromBottom: Option[A] = if(!postQueue.isEmpty) Some(postQueue.dequeue) else None
}
