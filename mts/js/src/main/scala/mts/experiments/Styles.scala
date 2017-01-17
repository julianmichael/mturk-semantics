package mts.experiments

import scalacss.DevDefaults._
import scala.language.postfixOps

object Styles extends StyleSheet.Inline {
  import dsl._

  val mainContent = style(
    font := "Helvetica"
  )

  val unselectable = style(
    userSelect := "none"
  )

  val answerIndicator = style(
    color(c"rgb(20, 180, 20)")
  )

  val listlessList = style(
    margin(0 px),
    padding(0 px),
    listStyleType := "none"
  )

  val questionWord = style(
    fontWeight.bold,
    textDecoration := "underline"
  )
}
