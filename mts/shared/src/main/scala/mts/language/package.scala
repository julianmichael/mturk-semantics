package mts

package object language extends PackagePlatformExtensions {
  def simpleTokenize(s: String): Vector[String] =
    s.split("(\\s+|[.,;!?.'\"])").toVector
}
