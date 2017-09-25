package turksem

import cats.data.NonEmptyList
import cats.Foldable
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import scala.util.{Try, Success, Failure}

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods, primarily for data analysis. */
package object util extends PackagePlatformExtensions {

  /** We require questions to begin with one of these words. */
  val whWords = Set("who", "what", "when", "where", "why", "how", "which", "whose").map(_.lowerCase)

  def beginsWithWh(s: String): Boolean = whWords.exists(w => s.toLowerCase.startsWith(w))

  def majorities[A](sets: Iterable[Set[A]]): Set[A] = {
    sets.flatten.toSet
      .filter(ai => sets.filter(_.contains(ai)).size >= (sets.size / 2))
  }

  def counts[F[_]: Foldable, A](fa: F[A]): Map[A, Int] =
    fa.foldLeft(Map.empty[A, Int].withDefaultValue(0)) {
      case (m, a) => m.updated(a, m(a) + 1)
    }

  def dollarsToCents(d: Double): Int = math.round(100 * d).toInt

  def longestCommonPrefix(s: String, t: String): String =
    (s, t).zipped.takeWhile(Function.tupled(_ == _)).map(_._1).mkString

  def percent(amount: Int, total: Int): Double =
    amount * 100.0 / total

  def pctString(num: Int, denom: Int): String =
    f"$num%d (${num * 100.0 / denom}%.2f%%)"
  def distString[N](iter: NonEmptyList[N])(implicit N : Numeric[N]): String =
    f"${N.toDouble(iter.sum)}%.2f (${iter.mean}%.2f ± ${iter.stdev}%.4f)"
  def noSumDistString[N](iter: NonEmptyList[N])(implicit N : Numeric[N]): String =
    f"${iter.mean}%.2f ± ${iter.stdev}%.4f"

  def const[A](a: A): Any => A = _ => a

  implicit class RichBoolean(val b: Boolean) extends AnyVal {
    def option[A](a: A) = if(b) Some(a) else None
    // TODO contribute this to mouse?
    import cats.Monoid
    def ifTrue[M](m: => M)(implicit M: Monoid[M]): M = if(b) m else M.empty
  }

  implicit class RichOption[A](val a: Option[A]) extends AnyVal {
    // more readable alternatives to forall/exists
    def emptyOr(predicate: A => Boolean): Boolean = a.forall(predicate)
    def nonEmptyAnd(predicate: A => Boolean): Boolean = a.exists(predicate)

    def ifEmpty[B](b: => B): Option[B] = a match {
      case Some(_) => None
      case None => Some(b)
    }
  }

  // TODO change to reducible
  implicit class RichNonEmptyList[A](val a: NonEmptyList[A]) extends AnyVal {
    def mean(implicit N: Numeric[A]): Double =
      N.toDouble(a.sum) / a.size

    def sse(implicit N: Numeric[A]): Double = {
      val m = a.mean
      a.map(x => math.pow(N.toDouble(x) - m, 2)).sum
    }

    def variance(implicit N: Numeric[A]) = a.sse / a.size

    def stdev(implicit N: Numeric[A]) = math.sqrt(a.variance)
  }

  implicit class RichFoldable[F[_]: Foldable, A](val fa: F[A]) {
    def sum(implicit N: Numeric[A]): A = fa.foldLeft(N.fromInt(0))(N.plus)

    def meanOpt(implicit N: Numeric[A]): Option[Double] = {
      val (sum, count) = fa.foldLeft(N.fromInt(0), N.fromInt(0)) {
        case ((curSum, curCount), a) => (N.plus(curSum, a), N.plus(curCount, N.fromInt(1)))
      }
      if(count == 0) None else Some(N.toDouble(sum) / N.toDouble(count))
    }

    // TODO other optional versions

    // def sse(implicit N: Numeric[A]): Double = {
    //   val m = a.mean
    //   a.map(x => math.pow(N.toDouble(x) - m, 2)).sum
    // }
    // def variance(implicit N: Numeric[A]) = a.sse / a.size
    // def varianceSample(implicit N: Numeric[A]) = a.sse / (a.size - 1)

    // def stdev(implicit N: Numeric[A]) = math.sqrt(a.variance)
    // def stdevSample(implicit N: Numeric[A]) = math.sqrt(a.varianceSample)
  }

  implicit class RichList[A](val as: List[A]) extends AnyVal {
    def remove(i: Int) = as.take(i) ++ as.drop(i + 1)
    def tailOption: Option[List[A]] = if(as.nonEmpty) Some(as.tail) else None
    def indexOpt(a: A): Option[Int] = Some(as.indexOf(a)).filter(_ >= 0)
    def collectFirstWithIndex[B](p: PartialFunction[A, B]): Option[(B, Int)] =
      as.zipWithIndex.collect {
        case (a, i) if p.isDefinedAt(a) => (p(a), i)
      }.headOption
    def findIndex(p: A => Boolean): Option[Int] = as.zipWithIndex.find(pair => p(pair._1)).map(_._2)
    // TODO doesn't short circuit when it finds the guy
    def indicesYielding[B](f: A => Option[B]): Seq[(Int, B)] =
      as.zipWithIndex.flatMap(pair => f(pair._1).map(b => (pair._2, b)))
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

  implicit class RichIterator[A](val t: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }
}
