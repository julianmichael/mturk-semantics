package turkey

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.collection.TraversableOnce

import scala.language.implicitConversions

/** Provides miscellaneous utility classes and methods.
  *
  * This includes mutable data structure (LazyStackQueue),
  * file management (FileManager),
  * and some random stuff (the latter three on this object).
  */
package object util extends PackagePlatformExtensions {

  protected[turkey] implicit class RichTry[A](val t: Try[A]) extends AnyVal {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) =>
        Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        e.printStackTrace()
        None
    }
  }

  protected[turkey] implicit class RichMutableStack[A](val s: mutable.Stack[A]) extends AnyVal {
    def popOption: Option[A] = if(!s.isEmpty) Some(s.pop) else None
  }

  protected[turkey] implicit class RichMutableQueue[A](val q: mutable.Queue[A]) extends AnyVal {
    def dequeueOption: Option[A] = if(!q.isEmpty) Some(q.dequeue) else None
  }

}
