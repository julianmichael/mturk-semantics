package mts.util

import scala.language.higherKinds

trait OrderedUnionFind[F[_]] {
  def empty[A : Ordering] : F[A]
  def add[A : Ordering](fa: F[A], a: A): F[A]
  def find[A](fa: F[A], a: A): Option[A]
  def union[A](fa: F[A], a: A, b: A): Option[F[A]]
  def representatives[A](fa: F[A]): Iterator[A]
}
