package turksem.util

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import monocle._
import monocle.macros._

object KeyboardControlComponent {

  @Lenses case class KeyboardControlState(isFocused: Boolean)

  case class KeyboardControlProps(
    handleKey: ReactKeyboardEvent => Callback,
    message: String,
    render: ReactTag)

  class KeyboardControlBackend(scope: BackendScope[KeyboardControlProps, KeyboardControlState]) {
    def render(props: KeyboardControlProps, state: KeyboardControlState) = <.div(
      ^.tabIndex := 0,
      ^.onFocus --> scope.modState(KeyboardControlState.isFocused.set(true)),
      ^.onBlur --> scope.modState(KeyboardControlState.isFocused.set(false)),
      ^.onKeyDown ==> props.handleKey,
      ^.position := "relative",
      !state.isFocused ?= <.div(
        ^.position := "absolute",
        ^.top := "20px",
        ^.left := "0px",
        ^.width := "100%",
        ^.height := "100%",
        ^.textAlign := "center",
        ^.color := "rgba(48, 140, 20, .3)",
        ^.fontSize := "48pt",
        props.message),
      props.render
    )
  }

  val KeyboardControl = ReactComponentB[KeyboardControlProps]("Keyboard Control")
    .initialState(KeyboardControlState(false))
    .renderBackend[KeyboardControlBackend]
    .build
}
