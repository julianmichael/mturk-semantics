package object mts extends PackagePlatformExtensions {

  implicit class HasTokensOps[A](a: A)(implicit ht: HasTokens[A]) {
    def tokens: Vector[String] = ht.getTokens(a)
  }

}
