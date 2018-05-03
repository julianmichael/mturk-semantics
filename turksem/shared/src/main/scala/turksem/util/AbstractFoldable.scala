package turksem.util

import cats.Eval
import cats.Foldable
import cats.Functor
import cats.implicits._

trait AbstractFoldable[A] {
  def foldLeft[B](b: B)(f: (B, A) => B): B
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
}
object AbstractFoldable {
  def fromIterator[A](makeIter: => Iterator[A]) = new AbstractFoldable[A] {
    override def foldLeft[B](b: B)(f: (B, A) => B): B = makeIter.foldLeft(b)(f)
    override def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = makeIter.foldRight(lb)(f)
  }

  def fromFoldable[F[_]: Foldable, A](fa: F[A]) = new AbstractFoldable[A] {
    override def foldLeft[B](b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    override def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
  }

  // plus other stuff like traverse, etc...
  implicit val abstractFoldableInstances = new Foldable[AbstractFoldable] with Functor[AbstractFoldable] {

    override def foldLeft[A, B](fa: AbstractFoldable[A], b: B)(f: (B, A) => B): B =
      fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: AbstractFoldable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.foldRight(lb)(f)

    override def map[A, B](fa: AbstractFoldable[A])(f: A => B): AbstractFoldable[B] =
      new AbstractFoldable[B] {

        override def foldLeft[C](c: C)(g: (C, B) => C): C =
          fa.foldLeft(c)((c, a) => g(c, f(a)))

        override def foldRight[C](lc: Eval[C])(g: (B, Eval[C]) => Eval[C]): Eval[C] =
          fa.foldRight(lc)((a, lc) => g(f(a), lc))

      }
  }
}
