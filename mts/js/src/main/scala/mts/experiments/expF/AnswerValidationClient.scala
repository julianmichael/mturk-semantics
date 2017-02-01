package mts.experiments.expF

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

object AnswerValidationClient extends TaskClient[ValidationPrompt, AnswerValidationResponse] {

  lazy val isNotAssigned = assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE"

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[ApiRequest, ApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int)]
  import HighlightingComponent._

  lazy val questions = prompt.sourcedQAPairs.map(_.question)

  @Lenses case class State(
    curQuestion: Int,
    isInterfaceFocused: Boolean)

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse(hs: HighlightingState): Callback = Callback {
      val questionIndices = (0 until questions.length)
      setSubmitEnabled(questionIndices.forall(qi => hs.span.exists(_._1 == qi)))
      setResponse(AnswerValidationResponse(questionIndices.toList.map(qi => hs.span.collect { case (`qi`, ai) => ai })))
    }

    def handleKey(e: ReactKeyboardEvent): Callback = {
      def next = scope.modState(State.curQuestion.modify(i => (i + 1) % questions.size))
      def prev = scope.modState(State.curQuestion.modify(i => (i + questions.size - 1) % questions.size))
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Down => next
        case KeyCode.Up => prev
      } >> e.preventDefaultCB
    }

    def qaField(curQuestion: Int, sentence: CoNLLSentence, span: Set[(Int, Int)])(index: Int) = {
      val isFocused = curQuestion == index
      val answerIndices = span.collect { case (`index`, ai) => ai }
      val isAnswerEmpty = answerIndices.isEmpty
      val curAnswer = TextRendering.renderSentence(
        sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))

      <.div(
        ^.overflow := "hidden",
        <.span(
          isFocused ?= Styles.bolded,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curQuestion.set(index)),
          questions(index)
        ),
        <.div(
          Styles.answerIndicator,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.width := "25px",
          isFocused ?= "-->"
        ),
        <.div(
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          isAnswerEmpty ?= (^.color := "#CCCCCC"),
          if(isAnswerEmpty && isFocused) {
            "Highlight your answer above, switch questions with arrow keys"
          } else {
            curAnswer
          }
        )
      )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri, request = SentenceRequest(prompt.path), render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(SentenceResponse(sentence), _) =>
              import scalaz.std.list._
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateResponse, render = {
                    case (HighlightingState(spans, status), HighlightingContext(startHighlight, startErase, stopHighlight, touchElement)) =>
                      val curSpan = spans.collect { case (questionIndex, i) if questionIndex == s.curQuestion => i }
                      def touchWord(i: Int) = touchElement((s.curQuestion, i))
                      <.div(
                        ^.onMouseUp --> stopHighlight,
                        ^.onMouseDown --> startHighlight,
                        Styles.mainContent,
                        instructions,
                        <.hr(),
                        <.div(
                          ^.tabIndex := 0,
                          ^.onFocus --> scope.modState(State.isInterfaceFocused.set(true)),
                          ^.onBlur --> scope.modState(State.isInterfaceFocused.set(false)),
                          ^.onKeyDown ==> handleKey,
                          ^.position := "relative",
                          !s.isInterfaceFocused ?= <.div(
                            ^.position := "absolute",
                            ^.top := "20px",
                            ^.left := "0px",
                            ^.width := "100%",
                            ^.height := "100%",
                            ^.textAlign := "center",
                            ^.color := "rgba(48, 140, 20, .3)",
                            ^.fontSize := "48pt",
                            "Click here to start"
                          ),
                          <.p(
                            Styles.unselectable,
                            TextRendering.renderSentence(
                              sentence.words,
                              getToken = (word: CoNLLWord) => word.token,
                              spaceFromNextWord = (nextWord: CoNLLWord) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(curSpan.contains(nextWord.index) && curSpan.contains(nextWord.index - 1)) {
                                      "#FFFF00"
                                    } else "transparent"),
                                  " ")),
                              renderWord = (word: CoNLLWord) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(curSpan.contains(word.index)) "#FFFF00"
                                    else "transparent"),
                                  ^.onMouseMove --> touchWord(word.index),
                                  ^.onMouseDown ==> (
                                    (e: ReactEventI) => if(curSpan.contains(word.index)) {
                                      e.stopPropagation // so we don't trigger the global startHighlight
                                      startErase >> touchWord(word.index)
                                    } else {
                                      startHighlight >> touchWord(word.index)
                                    }
                                  ),
                                  TextRendering.normalizeToken(word.token)
                                ))
                            )),
                          <.ul(
                            Styles.listlessList,
                            (0 until questions.size)
                              .map(qaField(s.curQuestion, sentence, spans))
                              .map(field => <.li(^.display := "block", field))
                          )
                        )
                      )
                  }
                )
              )
          }
        )
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(State(0, false))
    .renderBackend[FullUIBackend]
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }


  def example(question: String, answer: String, isGood: Boolean, tooltip: String) =
    <.li(
      <.div(
        ^.className := "tooltip",
        <.span(question),
        <.span(" --> "),
        <.span(
          isGood ?= Styles.goodGreen,
          !isGood ?= Styles.badRed,
          answer),
        <.span(^.className := "tooltiptext", tooltip)
      )
    )


  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into a list of questions and answers.
           You will be presented with a selection of English text and a list of questions prepared by other annotators."""),
    <.p("""You will highlight the smallest set of words in the sentence that correctly and naturally answers the question.
           For example, consider the following sentence and questions:"""),
    <.blockquote(<.i("The jubilant protesters celebrated after executive intervention canceled the project.")),
    <.ul(
      <.li("How did the protesters feel?"),
      <.li("Who celebrated?")),
    <.p("""You should answer the first question with """, <.b("jubilant "),
        """and the second question with """, <.b("The protesters.")),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Each of your answers must satisfy the following criteria:"""),
    <.ol(
      <.li("""It is a correct and natural answer to the question."""),
      <.li("""It is grammatical, to the extent possible."""),
      <.li("""It is the shortest such answer, i.e., it does not contain any unnecessary words.""")),
    <.p(""" If it is clear what the question is trying to ask (even if there are grammatical errors),
        please answer it to the best of your ability.
        If there are multiple correct answers, choose any one of them.
        If the question is incoherent, has no apparent connection to the sentence,
        or is otherwise unanswerable, you should mark it """, <.b("Invalid"), """.
        See the examples for further explanation."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i("""I take full and complete responsibility for
                        my thoughtless decision to disclose these materials to the public. """)),
    <.p("""Here are examples of answers someone might give to several questions.
           Good answers are in """, <.span(Styles.goodGreen, "green "), """and bad answers
           are in """, <.span(Styles.badRed, "red"), ". Mouse over each for an explanation."),
    <.ul(
      example(question = "Who decided to disclose materials to the public?", answer = "I", isGood = true,
              tooltip = """To deal with pronouns like "I", you should choose answers from the perspective of the speaker."""),
      example(question = "What is someone responsible for?", answer = "my decision", isGood = true,
              tooltip = """You should skip over words or phrases when they are not necessary to answer the question."""),
      example(question = "What am I taking?", answer = "responsibility for my decision", isGood = false,
              tooltip = """You should prefer the shortest natural, grammatical answer.
                           In this case it would be "responsibility." """)
    ),
    <.p("""If you consistently provide answers that violate the above criteria, you will be banned from this task and future tasks.
        Otherwise, your work will be approved within an hour."""),
    <.p("""Sometimes you may be unsure whether to choose between a shorter answer and a longer, slightly more natural one.
        Exercise your best judgment and as long as you choose something reasonable you will not be penalized.
        As a rule of thumb, when all else is equal, choose the shorter answer."""),
    <.p("""You may see the same or very similar questions appear twice. This is fine: just give the same answer."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field so we may improve the task.""")
  )
}
