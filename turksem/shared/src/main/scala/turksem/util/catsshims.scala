package turksem.util

import cats.Order

// remove when updating cats to latest
object catsshims {
  implicit class RichOrderObject(val o: Order.type) extends AnyVal {
    def whenEqual[A](first: Order[A], second: Order[A]): Order[A] =
      new Order[A] {
        def compare(x: A, y: A) = {
          val c = first.compare(x, y)
          if (c == 0) second.compare(x, y)
          else c
        }
      }
  }
}
