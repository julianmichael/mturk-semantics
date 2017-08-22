package turksem.util

import org.scalajs.dom.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import nlpdata.util.Text

import cats.implicits._

// only higher-order-ish. More for just reducing redundancy.
// feel free to abstract more stuff out of this if you need to.
object HighlightableSentenceComponent {

  case class HighlightableSentenceProps(
    sentence: Vector[String], // PTB tokens
    styleForIndex: Int => TagMod,
    highlightedIndices: Set[Int],
    startHighlight: Callback,
    startErase: Callback,
    touchWord: Int => Callback,
    render: List[VdomTagOf[html.Span]] => VdomElement) // word/span elements to whole thing

  val HighlightableSentence = ScalaComponent.builder[HighlightableSentenceProps]("Highlightable Sentence")
    .render_P {
    case HighlightableSentenceProps(sentence, styleForIndex, highlightedIndices, startHighlight, startErase, touchWord, render) =>
      val spans = Text.render(
        sentence.indices.toList,
        (index: Int) => sentence(index),
        (nextIndex: Int) => List(
          <.span(
            ^.backgroundColor := (
              if(highlightedIndices.contains(nextIndex) && highlightedIndices.contains(nextIndex - 1)) {
                "#FFFF00"
              } else {
                "transparent"
              }),
            " ")),
        (index: Int) => List(
          <.span(
            styleForIndex(index),
            ^.backgroundColor := (
              if(highlightedIndices.contains(index)) {
                "#FFFF00"
              } else {
                "transparent"
              }
            ),
            ^.onMouseMove --> touchWord(index),
            ^.onMouseDown ==> (
              (e: ReactEvent) => if(highlightedIndices.contains(index)) {
                e.stopPropagation
                startErase >> touchWord(index)
              } else {
                startHighlight >> touchWord(index)
              }
            ),
            Text.normalizeToken(sentence(index))
          ))
      )
      render(spans)
  }.build
}
