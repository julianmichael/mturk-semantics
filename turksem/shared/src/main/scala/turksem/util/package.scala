package turksem

import cats.Order
import cats.Foldable
import cats.Reducible
import cats.Traverse
import cats.data.NonEmptyList
import cats.data.State
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import scala.util.{Try, Success, Failure}
import scala.util.Random

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods, primarily for data analysis. */
package object util extends PackagePlatformExtensions {

  /** We require questions to begin with one of these words. */
  val whWords = Set("who", "what", "when", "where", "why", "how", "which", "whose").map(_.lowerCase)

  def beginsWithWh(s: String): Boolean = whWords.exists(w => s.toLowerCase.startsWith(w))

  def beginsWithWhSpace(s: String): Boolean = whWords.exists(w => s.lowerCase.startsWith(w + " ".lowerCase))

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
  def distString[F[_]: Reducible, N](iter: F[N])(implicit N : Numeric[N]): String =
    f"${N.toDouble(iter.sum)}%.2f (${iter.mean}%.2f ± ${iter.stdev}%.4f)"
  def noSumDistString[F[_]: Reducible, N](iter: F[N])(implicit N : Numeric[N]): String =
    f"${iter.mean}%.2f ± ${iter.stdev}%.4f"

  def const[A](a: A): Any => A = _ => a

  object IntMatch {
    def unapply(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
  }

  // for when a list is already nearly sorted
  // def insertionSort[A : Order](la: List[A]) = la.foldLeft(List.empty[A]) {
  //   case (reverseSortedSoFar, nextElem) =>
  //     val (greater, lesser) = reverseSortedSoFar.partition(_ > nextElem)
  //     greater ++ (nextElem :: lesser) // if already nearly sorted, greater will often be empty
  // }.reverse

  def mergeSortedLists[A : Order](xs: List[A], ys: List[A]): List[A] = {
    val (withAllXs, remGreaterYs) = xs.foldLeft((List.empty[A], ys)) {
      case ((acc, Nil), x) => (x :: acc, Nil)
      case ((acc, ysRemaining), x) =>
        val (lesserYs, greaterYs) = ysRemaining.partition(_ < x)
        val accWithLesserYs = lesserYs.foldLeft(acc) { case (a, y) => y :: a }
        (x :: accWithLesserYs, greaterYs)
    }
    remGreaterYs.foldLeft(withAllXs) { case (acc, y) => y :: acc }.reverse
  }

  import scala.annotation.tailrec

  @tailrec private def weightedRoundRobinAux[A](soFar: Vector[A], vectors: List[Vector[A]]): Vector[A] = {
      if(vectors.isEmpty) soFar else { // hit base case because filter out empties
        val smallestSize = vectors.map(_.size).min // works bc nonempty
        val (processedRemains, newSoFar) = vectors.foldLeft((List.empty[Vector[A]], soFar)) {
          case ((remains, fullSoFar), vector) =>
            val sizeMultiplier = vector.size / smallestSize
            (vector.drop(sizeMultiplier) :: remains, fullSoFar ++ vector.take(sizeMultiplier))
        }
        weightedRoundRobinAux(newSoFar, processedRemains.reverse.filter(_.nonEmpty))
      }
    }
  def weightedRoundRobin[A](vectors: List[Vector[A]]) = weightedRoundRobinAux(Vector.empty[A], vectors)

  @tailrec private def weightedRoundRobinRandomizedAux[A](
    soFar: Vector[A],
    vectors: List[Vector[A]],
    rand: Random
  ): Vector[A] = {
    if(vectors.isEmpty) soFar else { // hit base case because filter out empties
      val smallestSize = vectors.map(_.size).min // works bc nonempty
      val (processedRemains, newSoFar) = vectors.foldLeft((List.empty[Vector[A]], Vector.empty[A])) {
        case ((remains, soFarAcc), vector) =>
          val sizeMultiplier = vector.size / smallestSize
          (vector.drop(sizeMultiplier) :: remains, soFarAcc ++ vector.take(sizeMultiplier))
      }
      weightedRoundRobinRandomizedAux(soFar ++ rand.shuffle(newSoFar), processedRemains.reverse.filter(_.nonEmpty), rand)
    }
  }
  def weightedRoundRobinRandomized[A](
    vectors: List[Vector[A]],
    rand: Random
  ) = weightedRoundRobinRandomizedAux(Vector.empty[A], vectors, rand)

  // reorders a vector into a result s.t. prefixes of the result are
  // (roughly) maximally evenly distributed over the original vector
  private def evenDistributionAux[A](vector: Vector[A]): Iterator[A] = {
    if(vector.size <= 3) {
      vector.iterator
    } else {
      val (firstHalf, secondHalf) = vector.splitAt(vector.size / 2)
      evenDistributionAux(firstHalf).zip(evenDistributionAux(secondHalf)).flatMap {
        case (x, y) => x :: y :: Nil
      }
    }
  }
  def evenDistribution[A](vector: Vector[A]) = evenDistributionAux(vector).toVector


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

  implicit class RichReducible[F[_]: Reducible, A](val a: F[A]) {

    def head: A = a.reduceLeft { case (a, _) => a }

    def last: A = a.reduceLeft { case (_, a) => a }

    def mean(implicit N: Numeric[A]): Double =
      N.toDouble(a.sum) / a.size

    def sse(implicit N: Numeric[A]): Double = {
      val m = a.mean
      a.foldMap(x => math.pow(N.toDouble(x) - m, 2))
    }

    def variance(implicit N: Numeric[A]) = a.sse / a.size

    def stdev(implicit N: Numeric[A]) = math.sqrt(a.variance)

    // TODO more principled
    def modesNel: NonEmptyList[A] = {
      NonEmptyList.fromList(a.modes).get
    }
  }

  implicit class RichFoldable[F[_]: Foldable, A](val fa: F[A]) {

    def abstractify: AbstractFoldable[A] = AbstractFoldable.fromFoldable(fa)

    // def counts: Map[A, Int] =
    //   fa.foldLeft(Map.empty[A, Int].withDefaultValue(0)) {
    //     case (m, a) => m.updated(a, m(a) + 1)
    //   }

    def getAtIndex(i: Int): Option[A] = fa.foldM[Either[A, ?], Int](i) {
      case (0, a) => Left(a)
      case (i, _) => Right(i - 1)
    }.swap.toOption

    def sum(implicit N: Numeric[A]): A = fa.foldLeft(N.fromInt(0))(N.plus)

    def product(implicit N: Numeric[A]): A = fa.foldLeft(N.fromInt(0))(N.times)

    def meanOpt(implicit N: Numeric[A]): Option[Double] = {
      val (sum, count) = fa.foldLeft(N.fromInt(0), N.fromInt(0)) {
        case ((curSum, curCount), a) => (N.plus(curSum, a), N.plus(curCount, N.fromInt(1)))
      }
      if(count == 0) None else Some(N.toDouble(sum) / N.toDouble(count))
    }

    def proportionOpt(predicate: A => Boolean): Option[Double] = fa.foldLeft((0, 0)) {
      case ((trues, total), a) =>
        if(predicate(a)) (trues + 1, total + 1)
        else (trues, total + 1)
    } match { case (trues, total) => if(total == 0) None else Some(trues.toDouble / total) }

    // unsafe
    def proportion(predicate: A => Boolean): Double = fa.foldLeft((0, 0)) {
      case ((trues, total), a) =>
        if(predicate(a)) (trues + 1, total + 1)
        else (trues, total + 1)
    } match { case (trues, total) => trues.toDouble / total }

    def headOption: Option[A] = fa.foldLeft(None: Option[A]) {
      case (None, a) => Some(a)
      case (head, _) => head
    }

    def lastOption: Option[A] = fa.foldLeft(None: Option[A]) {
      case (None, a) => Some(a)
      case (_, a) => Some(a)
    }

    def modes: List[A] = {
      NonEmptyList.fromList(fa.toList.groupBy(identity).toList.sortBy(-_._2.size)).map { nel =>
        nel.filter(_._2.size == nel.head._2.size)
      }.foldK.map(_._1)
    }

    def groupSecondByFirst[B, C](implicit ev: (A =:= (B, C))): Map[B, List[C]] = {
      fa.toList.groupBy(_._1).map { case (k, ps) => k -> ps.map(_._2) }
    }

    // TODO other optional versions

    def sseOpt(implicit N: Numeric[A]): Option[Double] = {
      fa.meanOpt.map(m =>
        abstractify.map(x => math.pow(N.toDouble(x) - m, 2)).sum
      )
    }
    def varianceOpt(implicit N: Numeric[A]) = fa.sseOpt.map(_ / fa.size)
    def varianceSampleOpt(implicit N: Numeric[A]) = fa.sseOpt.map(_ / (fa.size - 1))

    def stdevOpt(implicit N: Numeric[A]) = fa.varianceOpt.map(math.sqrt)
    def stdevSampleOpt(implicit N: Numeric[A]) = fa.varianceSampleOpt.map(math.sqrt)
  }

  implicit class RichTraverse[F[_]: Traverse, A](val fa: F[A]) {

    def mapWithIndex[B](f: (A, Int) => B): F[B] =
      fa.traverse[State[Int, ?], B](a => State((s: Int) => (s + 1, f(a, s)))).runA(0).value

    def zipWithIndex: F[(A, Int)] =
      fa.mapWithIndex(_ -> _)

    def mapWithZippedList[B, C](bs: List[B], f: (A, Option[B]) => C): F[C] =
      fa.traverse[State[List[B], ?], C](a =>
        State((l: List[B]) =>
          l match {
            case Nil => (Nil, f(a, None))
            case x :: xs => (xs, f(a, Some(x)))
          })
      ).runA(bs).value

    def zipWithList[B](bs: List[B]): F[(A, Option[B])] = mapWithZippedList(bs, (_: A, _: Option[B]))
  }

  implicit class RichList[A](val as: List[A]) extends AnyVal {
    def remove(i: Int) = as.take(i) ++ as.drop(i + 1)
    def tailOption: Option[List[A]] = if(as.nonEmpty) Some(as.tail) else None
    def indexOpt(a: A): Option[Int] = Some(as.indexOf(a)).filter(_ >= 0)
    def collectWithIndex[B](f: PartialFunction[A, B]) =
      as.zipWithIndex.flatMap(p => f.lift(p._1).map(_ -> p._2))
    // TODO define in terms of above
    def collectFirstWithIndex[B](p: PartialFunction[A, B]): Option[(B, Int)] =
      as.zipWithIndex.collect {
        case (a, i) if p.isDefinedAt(a) => (p(a), i)
      }.headOption
    def findIndex(p: A => Boolean): Option[Int] =
      as.zipWithIndex.find(pair => p(pair._1)).map(_._2)
    def collectIndices(f: PartialFunction[A, Boolean]) =
      collectWithIndex(f).map(_._2)
    def collectIndex(f: PartialFunction[A, Boolean]) =
      collectIndices(f).headOption
    // TODO doesn't short circuit when it finds the guy
    // TODO phase out for collect methods above
    def indicesYielding[B](f: A => Option[B]): Seq[(Int, B)] =
      as.zipWithIndex.flatMap(pair => f(pair._1).map(b => (pair._2, b)))

    def collectPrefix[B](f: PartialFunction[A, B]): List[B] = {
      as.takeWhile(f.isDefinedAt).map(f)
    }

    def uncons: Option[(A, List[A])] = as match {
      case Nil => None
      case x :: xs => Some(x -> xs)
    }
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
    def unfoldListPartial[B](f: PartialFunction[A, (B, A)]): List[B] = a.unfoldList(f.lift)
  }

  implicit class RichIterator[A](val t: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }
}
