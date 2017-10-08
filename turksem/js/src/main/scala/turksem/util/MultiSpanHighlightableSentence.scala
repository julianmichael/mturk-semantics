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
object MultiSpanHighlightableSentenceComponent {

  case class MultiSpanHighlightableSentenceProps(
    sentence: Vector[String], // PTB tokens
    styleForIndex: Int => TagMod,
    highlightedSpans: List[(Set[Int], TagMod)], // in order of priority, each with own style. first determines click events
    startHighlight: Callback,
    startErase: Callback,
    touchWord: Int => Callback,
    render: List[VdomTagOf[html.Span]] => VdomElement) // word/span elements to whole thing

  val MultiSpanHighlightableSentence = ScalaComponent.builder[MultiSpanHighlightableSentenceProps]("Multi-Span Highlightable Sentence")
    .render_P {
    case MultiSpanHighlightableSentenceProps(sentence, styleForIndex, highlightedSpans, startHighlight, startErase, touchWord, render) =>
      val spans = Text.render(
        sentence.indices.toList,
        (index: Int) => sentence(index),
        (nextIndex: Int) => List(
          <.span(
            ^.key := s"space-$nextIndex",
            highlightedSpans.find(span => span._1.contains(nextIndex) && span._1.contains(nextIndex - 1)).whenDefined(_._2),
            " ")),
        (index: Int) => List(
          <.span(
            ^.key := s"word-$index",
            styleForIndex(index),
            highlightedSpans.find(span => span._1.contains(index)).whenDefined(_._2),
            ^.onMouseMove --> touchWord(index),
            ^.onMouseDown ==> (
              (e: ReactEvent) => if(highlightedSpans.headOption.nonEmptyAnd(_._1.contains(index))) {
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
