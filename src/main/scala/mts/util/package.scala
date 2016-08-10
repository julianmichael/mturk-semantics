package mts

import scala.util.{Try, Success, Failure}

package object util {
  lazy val stopwords: Set[String] =
    io.Source.fromFile("english.stop.txt", "iso-8859-1").getLines.toSet ++ Set("hm", "uh", "um")

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
}
