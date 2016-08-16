package mts

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.collection.TraversableOnce

package object util {
  lazy val stopwords: Set[String] =
    io.Source.fromFile("english.stop.txt", "iso-8859-1").getLines.toSet ++ Set("hm", "uh", "um")

  implicit class RichValForOptions[A](val a: A) extends AnyVal {
    def onlyIf(p: (A => Boolean)): Option[A] = Some(a).filter(p)
  }

  implicit class RichTry[A](val t: Try[A]) extends AnyVal {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) => Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
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