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
  // NOTE: there are other projects doing refinement typing...but they seem heavier weight for the user
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
}
