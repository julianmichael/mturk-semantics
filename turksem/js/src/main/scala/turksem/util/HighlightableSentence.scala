package turksem.util

import org.scalajs.dom.html

import japgolly.scalajs.react.vdom.prefix_<^._
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
    specialWordIndices: Set[Int],
    highlightedIndices: Set[Int],
    startHighlight: Callback,
    startErase: Callback,
    touchWord: Int => Callback,
    render: List[ReactTagOf[html.Span]] => ReactElement) // word/span elements to whole thing

  val HighlightableSentence = ReactComponentB[HighlightableSentenceProps]("Highlightable Sentence")
    .render_P {
    case HighlightableSentenceProps(sentence, specialWordIndices, highlightedIndices, startHighlight, startErase, touchWord, render) =>
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
            specialWordIndices.contains(index) ?= Styles.specialWord,
            specialWordIndices.contains(index) ?= Styles.niceBlue,
            ^.backgroundColor := (
              if(highlightedIndices.contains(index)) {
                "#FFFF00"
              } else {
                "transparent"
              }
            ),
            ^.onMouseMove --> touchWord(index),
            ^.onMouseDown ==> (
              (e: ReactEventI) => if(highlightedIndices.contains(index)) {
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
