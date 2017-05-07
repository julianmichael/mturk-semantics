package mts

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.collection.TraversableOnce

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods.
  *
  * This includes mutable data structures (LazyStackQueue, QueueMap, Counter),
  * file management (FileManager), text rendering (TextRendering),
  * type-level lowercase strings, extension methods for Scala stdlib types,
  * and some random stuff (the latter three on this object).
  */
package object util extends PackagePlatformExtensions {

  def majorities[A](sets: Iterable[Set[A]]): Set[A] = {
    sets.flatten.toSet
      .filter(ai => sets.filter(_.contains(ai)).size >= (sets.size / 2))
  }

  // == typeclass instances ==

  import scala.collection.immutable.SortedSet
    // basic, inefficient immutable union-find based on sets
  implicit object SetBasedUnionFind extends OrderedUnionFind[({ type λ[A] = Set[SortedSet[A]]})#λ] {
    override def empty[A : Ordering] =
      Set.empty[SortedSet[A]]

    override def add[A: Ordering](fa: Set[SortedSet[A]], a: A): Set[SortedSet[A]] =
      if(fa.forall(!_.contains(a))) fa + SortedSet[A](a)
      else fa

    override def find[A](fa: Set[SortedSet[A]], a: A): Option[A] =
      fa.find(_.contains(a)).map(_.head)

    override def union[A](fa: Set[SortedSet[A]], a: A, b: A): Option[Set[SortedSet[A]]] = for {
      aSet <- fa.find(_.contains(a))
      bSet <- fa.find(_.contains(b))
    } yield if(aSet != bSet) {
      fa - aSet - bSet + (aSet ++ bSet)
    }
    else fa

    override def representatives[A](fa: Set[SortedSet[A]]): Iterator[A] = fa.iterator.map(_.head)
  }

  // == smart matchers ==

  object IntMatch {
    val IntMatchRegex = "(\\d+)".r
    def unapply(s: String): Option[Int] = s match {
      case IntMatchRegex(num) => Some(num.toInt)
      case _ => None
    }
  }

  // == Extension methods ==

  def pctString(num: Int, denom: Int): String =
    f"$num%d (${num * 100.0 / denom}%.2f%%)"
  def distString[N](iter: Seq[N])(implicit N : Numeric[N]): String =
    f"${N.toDouble(iter.sum)}%.2f (${iter.mean}%.2f ± ${iter.stdev}%.4f)"
  def noSumDistString[N](iter: Seq[N])(implicit N : Numeric[N]): String =
    f"${iter.mean}%.2f ± ${iter.stdev}%.4f"

  def const[A](a: A): Any => A = _ => a

  // TODO make this return an option
  implicit class RichSeq[A](val a: Seq[A]) extends AnyVal {
    def mean(implicit N: Numeric[A]): Double = N.toDouble(a.sum) / a.size
    def sse(implicit N: Numeric[A]): Double = {
      val m = a.mean
      a.map(x => math.pow(N.toDouble(x) - m, 2)).sum
    }
    def variance(implicit N: Numeric[A]) = a.sse / a.size
    def varianceSample(implicit N: Numeric[A]) = a.sse / (a.size - 1)

    def stdev(implicit N: Numeric[A]) = math.sqrt(a.variance)
    def stdevSample(implicit N: Numeric[A]) = math.sqrt(a.varianceSample)
  }

  implicit class RichList[A](val a: List[A]) extends AnyVal {
    def remove(i: Int) = a.take(i) ++ a.drop(i + 1)
  }

  implicit class RichValForOptions[A](val a: A) extends AnyVal {
    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
    def ifNot(p: (A => Boolean)): Option[A] = Some(a).filterNot(p)
    def wrapNullable: Option[A] = if(a == null) None else Some(a) // TODO probably Option(A) works here
  }

  implicit class RichValForFunctions[A](val a: A) extends AnyVal {
    def <|[B] (f: A => B): B = f(a)
  }

  implicit class RichValForLists[A](val a: A) extends AnyVal {
    def unfoldList[B](f: A => Option[(B, A)]): List[B] = f(a) match {
      case None => Nil
      case Some((head, tailToGo)) => head :: tailToGo.unfoldList(f)
    }
    def unfoldList[B](f: PartialFunction[A, (B, A)]): List[B] = a.unfoldList(f.lift)
  }

  implicit class RichTry[A](val t: Try[A]) extends AnyVal {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) =>
        Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        e.printStackTrace()
        None
    }
  }

  implicit class RichIterator[A](val t: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }

  implicit class RichMutableStack[A](val s: mutable.Stack[A]) extends AnyVal {
    def popOption: Option[A] = if(!s.isEmpty) Some(s.pop) else None
  }

  implicit class RichMutableQueue[A](val q: mutable.Queue[A]) extends AnyVal {
    def dequeueOption: Option[A] = if(!q.isEmpty) Some(q.dequeue) else None
  }

  def dollarsToCents(d: Double): Int = math.round(100 * d).toInt

  def longestCommonPrefix(s: String, t: String): String =
    (s, t).zipped.takeWhile(Function.tupled(_ == _)).map(_._1).mkString

  def percent(amount: Int, total: Int): Double =
    amount * 100.0 / total
}
