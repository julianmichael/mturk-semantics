package mts

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.collection.TraversableOnce

import scala.language.implicitConversions

package object util {

  // Welcome to the new world.
  // The world of ad-hoc refinement types requiring nothing more from the user than a single method call.
  // NO MORE WILL YOU BE UNCERTAIN, ON THE FIRST LINE OF YOUR METHOD,
  // WHETHER THE STRING WAS GUARANTEED TO BE LOWERCASE.
  // FOR YOU HAVE GUARANTEED IT ALREADY IN THE TYPE SYSTEM.
  // This is your weapon. This is your LowerCaseString.
  // Wield it with pride.
  // NOTE: there are projects to help you do refinement typing...but they seem a bit heavier weight for client code...idk
  // Anyway, don't try to read the code just below. The point is that you can write:
  // import mts.util.LowerCaseStrings._
  // and then you get the _.lowerCase method on strings, which yields a LowerCaseString,
  // as well as an implicit conversion from LowerCaseString back to String.
  // In addition, certain uses of existing methods on String will preserve LowerCaseString (as of now, just +);
  // if you want there to be more, feel free to let me (Julian) know and I can add them here.
  // I know it seems like weird extra complication, but honestly I was already having bugs from not lowercasing strings,
  // despite sprinkling calls to .toLowerCase around so much that the code had gotten noticeably harder to read.
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

  // Methods I wish were there on existing types. Now they are!

  implicit class RichValForOptions[A](val a: A) extends AnyVal {
    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
    def ifNot(p: (A => Boolean)): Option[A] = Some(a).filterNot(p)
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

  // Random utility methods.

  implicit def pairwise[A, B, C](f: (A, B) => C): (((A, B)) => C) =
    (p: (A, B)) => f(p._1, p._2)

  def sendToClipboard(s: String): Unit = {
    import java.awt._;
    import java.awt.datatransfer._;
    import java.io._;
    val selection = new StringSelection(s)
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, selection)
  }

  def counts[T](xs: TraversableOnce[T]): Map[T, Int] = {
    val m = mutable.HashMap.empty[T, Int].withDefaultValue(0)
    xs.foreach(m(_) += 1)
    m.toMap
  }

  val javaScriptIndexOf = """
// Production steps of ECMA-262, Edition 5, 15.4.4.14
// Reference: http://es5.github.io/#x15.4.4.14
if (!Array.prototype.indexOf) {
  Array.prototype.indexOf = function(searchElement, fromIndex) {

    var k;

    // 1. Let o be the result of calling ToObject passing
    //    the this value as the argument.
    if (this == null) {
      throw new TypeError('"this" is null or not defined');
    }

    var o = Object(this);

    // 2. Let lenValue be the result of calling the Get
    //    internal method of o with the argument "length".
    // 3. Let len be ToUint32(lenValue).
    var len = o.length >>> 0;

    // 4. If len is 0, return -1.
    if (len === 0) {
      return -1;
    }

    // 5. If argument fromIndex was passed let n be
    //    ToInteger(fromIndex); else let n be 0.
    var n = +fromIndex || 0;

    if (Math.abs(n) === Infinity) {
      n = 0;
    }

    // 6. If n >= len, return -1.
    if (n >= len) {
      return -1;
    }

    // 7. If n >= 0, then Let k be n.
    // 8. Else, n<0, Let k be len - abs(n).
    //    If k is less than 0, then let k be 0.
    k = Math.max(n >= 0 ? n : len - Math.abs(n), 0);

    // 9. Repeat, while k < len
    while (k < len) {
      // a. Let Pk be ToString(k).
      //   This is implicit for LHS operands of the in operator
      // b. Let kPresent be the result of calling the
      //    HasProperty internal method of o with argument Pk.
      //   This step can be combined with c
      // c. If kPresent is true, then
      //    i.  Let elementK be the result of calling the Get
      //        internal method of o with the argument ToString(k).
      //   ii.  Let same be the result of applying the
      //        Strict Equality Comparison Algorithm to
      //        searchElement and elementK.
      //  iii.  If same is true, return k.
      if (k in o && o[k] === searchElement) {
        return k;
      }
      k++;
    }
    return -1;
  };
}"""
}
