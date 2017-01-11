package mts.experiments.sample

import scalacss.DevDefaults._
import scala.language.postfixOps

object Styles extends StyleSheet.Inline {
  import dsl._

  val mainContent = style(
    font := "Helvetica"
  )

}
