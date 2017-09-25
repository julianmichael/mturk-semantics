package turksem.util

sealed trait ContiguousSpan {
  def begin: Int
  def end: Int

  def length = end - begin + 1
  def indices = (begin to end).toSet
  def contains(i: Int) = begin <= i && i <= end
  def containsSpaceBefore(i: Int) = begin <= (i - 1) && i <= end
}

case class ContiguousSpanImpl(
  override val begin: Int,
  override val end: Int) extends ContiguousSpan {
  override def toString = s"ContiguousSpan($begin, $end)"
}

object ContiguousSpan {

  import upickle.default._
  implicit val contiguousSpanReadWriter: ReadWriter[ContiguousSpan] =
    macroRW[ContiguousSpanImpl] merge[ContiguousSpanImpl, ContiguousSpan] macroRW[ContiguousSpanImpl]
  // spurious extra case just to get the types to work out ^

  def apply(x: Int, y: Int): ContiguousSpan = ContiguousSpanImpl(math.min(x, y), math.max(x, y))
  def unapply(cs: ContiguousSpan): Option[(Int, Int)] = ContiguousSpanImpl.unapply(cs.asInstanceOf[ContiguousSpanImpl])

  def fromSet(s: Set[Int]) = ContiguousSpan(s.min, s.max)
}

