package mts.experiments.expF

import mts.experiments._
import mts.datasets.conll._
import mts.tasks._
import mts.language._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import autowire._
import upickle.default._

import monocle._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

class HighlightingComponent[Index]() {

  sealed trait HighlightingStatus
  case object DoNothing extends HighlightingStatus
  case object Highlight extends HighlightingStatus
  case object Erase extends HighlightingStatus

  @Lenses case class HighlightingState(
    span: Set[Index],
    status: HighlightingStatus)
  object HighlightingState {
    def init(is: Set[Index]) = HighlightingState(is, DoNothing)
  }

  case class HighlightingContext(
    startHighlight: Callback,
    startErase: Callback,
    stop: Callback,
    touchElement: Index => Callback)

  case class HighlightingProps(
    isEnabled: Boolean,
    update: HighlightingState => Callback,
    initial: Set[Index] = Set.empty[Index],
    render: (HighlightingState, HighlightingContext) => ReactElement)

  class HighlightingBackend(scope: BackendScope[HighlightingProps, HighlightingState]) {

    def touchElement(props: HighlightingProps)(index: Index): Callback = scope.modState {
      case s @ HighlightingState(span, status) =>
        if(!props.isEnabled) s
        else status match {
          case DoNothing => s
          case Highlight => HighlightingState(span + index, status)
          case Erase => HighlightingState(span - index, status)
        }
    } >> scope.state.flatMap(s => scope.props.flatMap(p => p.update(s)))
    def setHighlightingStatus(s: HighlightingStatus): Callback =
      scope.modState(HighlightingState.status.set(s)) >>
        scope.state.flatMap(s => scope.props.flatMap(p => p.update(s)))
    val startHighlight: Callback = setHighlightingStatus(Highlight)
    val startErase: Callback = setHighlightingStatus(Erase)
    val stop: Callback = setHighlightingStatus(DoNothing)

    def render(props: HighlightingProps, state: HighlightingState) =
      props.render(state, HighlightingContext(startHighlight, startErase, stop, touchElement(props)))
  }

  val Highlighting = ReactComponentB[HighlightingProps]("Highlighting")
    .initialState_P(props => HighlightingState.init(props.initial))
    .renderBackend[HighlightingBackend]
    .build
}
