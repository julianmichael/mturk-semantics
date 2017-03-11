package mts.experiments.expF

import mts.experiments._
import mts.datasets.conll._
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

object LongAnswerValidationClient extends TaskClient[ValidationPrompt, AnswerValidationResponse] {

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[ApiRequest, ApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int)]
  import HighlightingComponent._

  lazy val questions = prompt.sourcedQAPairs.map(_.question)

  @Lenses case class State(
    curQuestion: Int,
    isInterfaceFocused: Boolean,
    invalids: Set[Int])

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse(state: State)(hs: HighlightingState): Callback = Callback {
      val questionIndices = (0 until questions.length)
      setSubmitEnabled(questionIndices.forall(qi => state.invalids.contains(qi) || hs.span.exists(_._1 == qi)))
      setResponse(
        AnswerValidationResponse(
          questionIndices.toList.map(
            qi => if(state.invalids.contains(qi)) None
                  else Some(hs.span.collect { case (`qi`, ai) => ai })
          )
        )
      )
    }

    def handleKey(e: ReactKeyboardEvent): Callback = {
      def next = scope.modState(State.curQuestion.modify(i => (i + 1) % questions.size))
      def prev = scope.modState(State.curQuestion.modify(i => (i + questions.size - 1) % questions.size))
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Down => next
        case KeyCode.Up => prev
      } >> e.preventDefaultCB
    }

    def qaField(curQuestion: Int, invalids: Set[Int], sentence: CoNLLSentence, span: Set[(Int, Int)])(index: Int) = {
      val isFocused = curQuestion == index
      val answerIndices = span.collect { case (`index`, ai) => ai }
      val isAnswerEmpty = answerIndices.isEmpty
      val curAnswer = TextRendering.renderSentence(
        sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))

      <.div(
        ^.overflow := "hidden",
        <.div(
          Styles.unselectable,
          ^.float := "left",
          // ^.`type` := "button",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "50px",
          invalids.contains(index) ?= (^.backgroundColor := "#E01010"),
          ^.onClick --> scope.modState(
            State.invalids.modify(is => if(is.contains(index)) is - index else is + index)
          ),
          "Invalid"
        ),
        <.span(
          isFocused ?= Styles.bolded,
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curQuestion.set(index)),
          questions(index)
        ),
        <.div(
          Styles.answerIndicator,
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.width := "25px",
          isFocused ?= "-->"
        ),
        <.div(
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          (invalids.contains(index) || isAnswerEmpty) ?= (^.color := "#CCCCCC"),
          if(invalids.contains(index)) {
            "N/A"
          } else if(isAnswerEmpty && isFocused) {
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
                  isEnabled = !isNotAssigned && !s.invalids(s.curQuestion), update = updateResponse(s), render = {
                    case (HighlightingState(spans, status), HighlightingContext(startHighlight, startErase, stopHighlight, touchElement)) =>
                      val showHighlights = !s.invalids(s.curQuestion)
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
                            (if(isNotAssigned) "Accept assignment to start" else "Click here to start")
                          ),
                          <.p(
                            Styles.unselectable,
                            TextRendering.renderSentence(
                              sentence.words,
                              getToken = (word: Word) => word.token,
                              spaceFromNextWord = (nextWord: Word) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(showHighlights && curSpan.contains(nextWord.index) && curSpan.contains(nextWord.index - 1)) {
                                      "#FFFF00"
                                    } else "transparent"),
                                  " ")),
                              renderWord = (word: Word) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(showHighlights && curSpan.contains(word.index)) "#FFFF00"
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
                              .map(qaField(s.curQuestion, s.invalids, sentence, spans))
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
    .initialState(State(0, false, Set.empty[Int]))
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
           You will be presented with a selection of English text and a list of at most 6 questions prepared by other annotators."""),
    <.p("""You will highlight the words in the sentence that correctly answer the question.
           For example, consider the following sentence and questions:"""),
    <.blockquote(<.i("The jubilant protesters celebrated after executive intervention canceled the project.")),
    <.ul(
      <.li("How did the protesters feel?"),
      <.li("When did someone celebrate?"),
      <.li("Who celebrated?")),
    <.p("""You should answer the first question with """, <.b("jubilant"),
        """, the second question with """, <.b("after executive intervention canceled the project"),
        """, and the third question with """, <.b("The jubilant protesters.")),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Each of your answers must satisfy the following criteria:"""),
    <.ol(
      <.li("""It is a correct answer to the question."""),
      <.li("""It is grammatical, to the extent possible.""")),
    <.p("""
        If there are multiple correct answers, include all of them in your answer if possible.
        (Skipping over words might help with this.)
        If the question is incoherent, has no obvious and grammatical answer in the sentence,
        or is otherwise unanswerable, you should mark it """, <.b("Invalid"), """.
        See the examples for further explanation."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i(""" "I take full and complete responsibility," she said, "for
                        my decision to disclose these materials to the public." """)),
    <.p("""Here are examples of answers someone might give to several questions.
           Good answers are in """, <.span(Styles.goodGreen, "green "), """and bad answers
           are in """, <.span(Styles.badRed, "red"), ". Mouse over each for an explanation."),
    <.ul(
      example(question = "Who decided to disclose materials to the public?", answer = "I", isGood = true,
              tooltip = """To deal with pronouns like "I", you should choose answers from the perspective of the speaker."""),
      example(question = "What is someone responsible for?", answer = "my decision", isGood = false,
              tooltip = """If shorter and longer answers both suffice, favor the longer one.
                        This answer should be "my decision to disclose these materials to the public"."""),
      example(question = "What did someone take?",
              answer = "responsibility for my decision to disclose these materials to the public",
              isGood = true,
              tooltip = """Skip over words if necessary to produce a complete and grammatical answer."""),
      example(question = "What did she say?",
              answer = "I take full and complete responsibility",
              isGood = false,
              tooltip = """Provide the most complete answer: this answer should be the entire quotation in the sentence
                        (skipping over "she said")."""),
      example(question = "Is it full responsibility or not?",
              answer = "full and complete",
              isGood = false,
              tooltip = """The answer should directly correspond to what the question is asking. Yes/no questions,
                        and questions that list the answers explicitly, should be marked invalid.""")
    ),
    <.p("""If you consistently provide answers that don't satisfy the listed criteria, you will be banned from this task.
        Otherwise, your work will be approved within an hour."""),
    <.p("""Sometimes you may not be sure whether to include some words in your answer.
        As a rule of thumb, include only what is necessary for the answer to be completely specified,
        but if all else is equal, prefer longer answers over shorter ones."""),
    <.p("""Also, you may see the same or very similar questions appear more than once. This is fine: just give the same answer."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field so we may improve the task.""")
  )
}
