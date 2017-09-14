package turksem.util

class DependentMap[F[_], G[_]] private (map: Map[F[_], G[_]]) {
  def get[A](key: F[A]): Option[G[A]] = map.get(key).asInstanceOf[Option[G[A]]]
  def put[A](key: F[A], value: G[A]): DependentMap[F, G] = new DependentMap[F, G](map + (key -> value).asInstanceOf[(F[_], G[_])])
  def remove[A](key: F[A]): DependentMap[F, G] = new DependentMap[F, G](map - key)
  def keys: Iterable[F[_]] = map.keys
  def size: Int = map.size
  override def toString = map.toString
}
object DependentMap {
  def empty[F[_], G[_]] = new DependentMap[F, G](Map.empty[F[_], G[_]])
}
