package nlpdata

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.collection.TraversableOnce

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods.
  *
  * This includes mutable data structures (LazyStackQueue, QueueMap, Counter),
  * file management (FileManager), text rendering (Text),
  * type-level lowercase strings, extension methods for Scala stdlib types,
  * and some random stuff (the latter three on this object).
  */
package object util extends PackagePlatformExtensions {
  def simpleTokenize(s: String): Vector[String] =
    s.split("(\\s+|[.,;!?.'\"])").toVector

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

  // == smart matchers ==

  object IntMatch {
    val IntMatchRegex = "(\\d+)".r
    def unapply(s: String): Option[Int] = s match {
      case IntMatchRegex(num) => Some(num.toInt)
      case _ => None
    }
  }

  // == Extension methods ==

  protected[nlpdata] def const[A](a: A): Any => A = _ => a

  // TODO make this return an option
  protected[nlpdata] implicit class RichSeq[A](val a: Seq[A]) extends AnyVal {
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

  protected[nlpdata] implicit class RichList[A](val a: List[A]) extends AnyVal {
    def remove(i: Int) = a.take(i) ++ a.drop(i + 1)
  }

  protected[nlpdata] implicit class RichValForOptions[A](val a: A) extends AnyVal {
    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
    def ifNot(p: (A => Boolean)): Option[A] = Some(a).filterNot(p)
    def wrapNullable: Option[A] = if(a == null) None else Some(a) // TODO probably Option(A) works here
  }

  protected[nlpdata] implicit class RichValForFunctions[A](val a: A) extends AnyVal {
    def <|[B] (f: A => B): B = f(a)
  }

  protected[nlpdata] implicit class RichValForLists[A](val a: A) extends AnyVal {
    def unfoldList[B](f: A => Option[(B, A)]): List[B] = f(a) match {
      case None => Nil
      case Some((head, tailToGo)) => head :: tailToGo.unfoldList(f)
    }
    def unfoldList[B](f: PartialFunction[A, (B, A)]): List[B] = a.unfoldList(f.lift)
  }

  protected[nlpdata] implicit class RichTry[A](val t: Try[A]) extends AnyVal {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) =>
        Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        e.printStackTrace()
        None
    }
  }

  protected[nlpdata] implicit class RichIterator[A](val t: Iterator[A]) extends AnyVal {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }

  protected[nlpdata] implicit class RichMutableStack[A](val s: mutable.Stack[A]) extends AnyVal {
    def popOption: Option[A] = if(!s.isEmpty) Some(s.pop) else None
  }

  protected[nlpdata] implicit class RichMutableQueue[A](val q: mutable.Queue[A]) extends AnyVal {
    def dequeueOption: Option[A] = if(!q.isEmpty) Some(q.dequeue) else None
  }
}
