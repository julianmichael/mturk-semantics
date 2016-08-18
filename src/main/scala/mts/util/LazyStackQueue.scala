package mts.util

import scala.collection.mutable

// "consumes" the iterator, but only traverses it when necessary to get new items
class LazyStackQueue[A](
  private[this] val source: Iterator[A],
  private[this] val bufferSize: Int = 10
) {

  private[this] val preStack: mutable.Stack[A] = mutable.Stack.empty[A]
  private[this] val midQueue: mutable.Queue[A] = mutable.Queue.empty[A]
  private[this] val postQueue: mutable.Queue[A] = mutable.Queue.empty[A]

  def push(a: A): Unit = preStack.push(a)
  def enqueue(a: A): Unit = postQueue.enqueue(a)

  def pop: Option[A] = popFromTop.orElse(popFromMiddle).orElse(popFromBottom)
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
