package mts

import scala.util.{Try, Success, Failure}

package object util {
  implicit class EnrichedTry[A](t: Try[A]) {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) => Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        None
    }
  }

  implicit class EnrichedIterator[A](t: Iterator[A]) {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }
}
