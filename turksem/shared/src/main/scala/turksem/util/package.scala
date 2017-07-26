package turksem

import scala.util.{Try, Success, Failure}

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods, primarily for data analysis. */
package object util extends PackagePlatformExtensions {

  def majorities[A](sets: Iterable[Set[A]]): Set[A] = {
    sets.flatten.toSet
      .filter(ai => sets.filter(_.contains(ai)).size >= (sets.size / 2))
  }

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

  def dollarsToCents(d: Double): Int = math.round(100 * d).toInt

  def longestCommonPrefix(s: String, t: String): String =
    (s, t).zipped.takeWhile(Function.tupled(_ == _)).map(_._1).mkString

  def percent(amount: Int, total: Int): Double =
    amount * 100.0 / total

  def pctString(num: Int, denom: Int): String =
    f"$num%d (${num * 100.0 / denom}%.2f%%)"
  def distString[N](iter: Seq[N])(implicit N : Numeric[N]): String =
    f"${N.toDouble(iter.sum)}%.2f (${iter.mean}%.2f ± ${iter.stdev}%.4f)"
  def noSumDistString[N](iter: Seq[N])(implicit N : Numeric[N]): String =
    f"${iter.mean}%.2f ± ${iter.stdev}%.4f"

  def const[A](a: A): Any => A = _ => a
}
