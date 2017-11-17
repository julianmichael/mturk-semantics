package turksem.util

import scala.annotation.tailrec

// totally not remotely lawful at all. Not even a lawful functor.
// just for debugging convenience.

import cats.Monad
import cats.implicits._

case class PrintingValue[A](value: A)
object PrintingValue {
  implicit val printingValueAlgebra: Monad[PrintingValue] = new Monad[PrintingValue] {
    override def pure[A](a: A): PrintingValue[A] = {
      println(a)
      PrintingValue(a)
    }

    override def map[A, B](fa: PrintingValue[A])(f: A => B): PrintingValue[B] = {
      val fb = f(fa.value)
      println(fb)
      PrintingValue(fb)
    }

    override def flatten[A](ffa: PrintingValue[PrintingValue[A]]): PrintingValue[A] =
      PrintingValue(ffa.value.value)

    override def flatMap[A, B](fa: PrintingValue[A])(f: A => PrintingValue[B]) =
      flatten(map(fa)(f))

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => PrintingValue[Either[A, B]]): PrintingValue[B] =
      f(a).value match {
        case Right(b) => println(b); PrintingValue(b)
        case Left(a2) => tailRecM[A, B](a2)(f)
      }
  }
}
