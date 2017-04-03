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

  /* Welcome to the new world.
   * The world of ad-hoc refinement types requiring nothing more from the user than a single method call.
   * NO MORE WILL YOU BE UNCERTAIN, ON THE FIRST LINE OF YOUR METHOD,
   * WHETHER THE STRING WAS GUARANTEED TO BE LOWERCASE.
   * FOR YOU HAVE GUARANTEED IT ALREADY IN THE TYPE SYSTEM.
   * This is your weapon. This is your LowerCaseString.
   * Wield it with pride.
   * NOTE: there are projects to help you do refinement typing...but they seem a bit heavier weight for client code...idk
   * Anyway, don't try to read the code just below. The point is that you can write:
   * import mts.util.LowerCaseStrings._
   * and then you get the _.lowerCase method on strings, which yields a LowerCaseString,
   * as well as an implicit conversion from LowerCaseString back to String.
   * In addition, certain uses of existing methods on String will preserve LowerCaseString (as of now, just +);
   * if you want there to be more, feel free to let me (Julian) know and I can add them here.
   * I know it seems like weird extra complication, but honestly I was already having bugs from not lowercasing strings,
   * despite sprinkling calls to .toLowerCase around so much that the code had gotten noticeably harder to read.
   */
  protected sealed trait LowerCaseStringCapsule0 {
    type LowerCaseString
    protected[util] sealed trait LowerCaseStringOps {
      def lowerCase(s: String): LowerCaseString
      def +(s1: LowerCaseString, s2: LowerCaseString): LowerCaseString
    }
    protected[util] val LowerCaseStringOpsImpl: LowerCaseStringOps
    implicit def lowerCaseStringToString(lcs: LowerCaseString): String
  }
  protected sealed trait LowerCaseStringCapsule extends LowerCaseStringCapsule0 {
    implicit def wrapLowerCaseString(lcs: LowerCaseString): LowerCaseStringWrapper
    implicit def wrapStringToMakeLowerCase(s: String): StringToLowerCaseWrapper
  }
  val LowerCaseStrings: LowerCaseStringCapsule = new LowerCaseStringCapsule {
    override type LowerCaseString = String
    protected[util] override object LowerCaseStringOpsImpl extends LowerCaseStringOps {
      override def lowerCase(s: String): LowerCaseString = s.toLowerCase
      override def +(s1: LowerCaseString, s2: LowerCaseString) = s1 + s2
    }
    override implicit def lowerCaseStringToString(lcs: LowerCaseString): String =
      lcs
    override implicit def wrapLowerCaseString(lcs: LowerCaseString) =
      new LowerCaseStringWrapper(lcs.asInstanceOf[LowerCaseStrings.LowerCaseString])
    override implicit def wrapStringToMakeLowerCase(s: String) =
      new StringToLowerCaseWrapper(s)
  }
  import LowerCaseStrings.LowerCaseString
  protected class LowerCaseStringWrapper(val lcs: LowerCaseString) extends AnyVal {
    def +(other: LowerCaseString): LowerCaseString = LowerCaseStrings.LowerCaseStringOpsImpl.+(lcs, other)
  }
  protected class StringToLowerCaseWrapper(val s: String) extends AnyVal {
    def lowerCase = LowerCaseStrings.LowerCaseStringOpsImpl.lowerCase(s)
  }

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

  // == Extension methods ==

  // TODO make this return an option
  implicit class RichSeq[A](val a: Seq[A]) extends AnyVal {
    def mean(implicit N: Numeric[A]) = N.toDouble(a.sum) / a.size
    def variance(implicit N: Numeric[A]) = {
      val m = a.mean
      a.map(x => math.pow(N.toDouble(x) - m, 2)).mean
    }
    def stdev(implicit N: Numeric[A]) = math.sqrt(a.variance)
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
