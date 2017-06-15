package object qamr {

  implicit class HasTokensOps[A](a: A)(implicit ht: HasTokens[A]) {
    def tokens: Vector[String] = ht.getTokens(a)
  }

}
