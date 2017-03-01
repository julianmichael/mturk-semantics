package mts.experiments

import scalacss.DevDefaults._
import scala.language.postfixOps

object Styles extends StyleSheet.Inline {
  import dsl._

  // freeze after expE

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

  val specialWord = style(
    fontWeight.bold,
    textDecoration := "underline"
  )

  val goodGreen = style(
    color(c"rgb(48, 140, 20)")
  )

  val badRed = style(
    color(c"rgb(216, 31, 00)")
  )

  // freeze after expF

  val bolded = style(fontWeight.bold)

  // freeze after expG

  val sideButton = style(
    float.left,
    margin(1 px),
    padding(1 px),
    width(25 px))

  val bottomButton = style(
    margin(1 px),
    padding(1 px),
    width(25 px))

  val niceBlue = style(
    style(fontWeight.bold),
    color(c"rgb(50, 164, 251)"))

  val greenBack = style(
    backgroundColor(c"rgba(48, 140, 20, 0.2)"))

  val paddingTop150 = style(
    paddingTop(150 px)
  )

  val topSep = style(
    borderBottom(1 px, solid, c"rgba(0, 0, 0, 0.3)")
  )

  val hoverBlueBold = style(
    &.hover(
      style(fontWeight.bold),
      color(c"rgb(50, 164, 251)")
    )
  )

  val hoverBold = style(
    &.hover(
      style(fontWeight.bold)
    )
  )

  val badRedBold = style(
    color(c"rgb(216, 31, 00)"),
    fontWeight.bold
  )
}
