package mts.experiments.expF

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._
import mts.util.dollarsToCents

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

import expE.{QAGenPrompt, QAGenResponse}

object QAValidationClient extends TaskClient[QAGenPrompt, QAGenResponse] {

  // val isNotAssigned = assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE"

  // sealed trait HighlightingState
  // case object DoingNothing extends HighlightingState
  // case object Highlighting extends HighlightingState
  // case object Erasing extends HighlightingState

  // sealed trait State
  // @Lenses case class Loading(
  //   message: String
  // ) extends State
  // @Lenses case class Loaded(
  //   sentence: CoNLLSentence,
  //   qaPairs: List[(String, Set[Int])],
  //   currentFocus: Int,
  //   highlightingState: HighlightingState
  // ) extends State {
  //   def specialWord = sentence.words(prompt.wordIndex)
  // }
  // object State {
  //   def loading[A]: Prism[State, Loading] = GenPrism[State, Loading]
  //   def loaded[A]: Prism[State, Loaded] = GenPrism[State, Loaded]
  // }

  // val currentFocusLens = State.loaded composeLens Loaded.currentFocus
  // val qaPairsLens = State.loaded composeLens Loaded.qaPairs
  // val highlightingStateLens = State.loaded composeLens Loaded.highlightingState

  // val emptyQA = ("", Set.empty[Int])

  // class FullUIBackend(scope: BackendScope[Unit, State]) {
  //   def load: Callback = scope.state map {
  //     case Loading(_) =>
  //       val socket = new dom.WebSocket(websocketUri)
  //       socket.onopen = { (event: Event) =>
  //         scope.setState(Loading("Retrieving data")).runNow
  //         socket.send(write(SentenceRequest(prompt.path)))
  //       }
  //       socket.onerror = { (event: ErrorEvent) =>
  //         val msg = s"Connection failure. Error code: ${event.colno}"
  //         System.err.println(msg)
  //         // TODO maybe retry or something
  //       }
  //       socket.onmessage = { (event: MessageEvent) ⇒
  //         val response = read[ApiResponse](event.data.toString)
  //         response match {
  //           case SentenceResponse(path, sentence) =>
  //             scope.setState(Loaded(sentence, List.fill(numQAs)(emptyQA), 0, DoingNothing)).runNow
  //         }
  //       }
  //       socket.onclose = { (event: Event) =>
  //         val msg = s"Connection lost."
  //         System.err.println(msg)
  //         // TODO maybe retry or something. probably not. right now this always happens because no heartbeat
  //       }
  //     case Loaded(_, _, _, _) =>
  //       System.err.println("Data already loaded.")
  //   }

  //   def updateResponse: Callback = scope.state.map { st =>
  //     setSubmitEnabled(qaPairsLens.getOption(st).map(_.exists { case (q, a) => !q.isEmpty && !a.isEmpty }).getOrElse(false))
  //     qaPairsLens.getOption(st).map(QAGenResponse.apply).foreach(setResponse)
  //   }

  //   def qaField(loadedState: Loaded, index: Int, bonus: Double) = loadedState match {
  //     case ls @ Loaded(sentence, qaPairs, currentFocus, _) =>
  //       val isFocused = loadedState.currentFocus == index
  //       val isAnswerEmpty = qaPairs(index)._2.isEmpty
  //       <.div(
  //         ^.overflow := "hidden",
  //         <.div(
  //           ^.float := "left",
  //           ^.width := "25px",
  //           ^.minHeight := "1px",
  //           ^.margin := "1px",
  //           ^.padding := "1px",
  //           ^.textAlign := "right",
  //           (bonus != 0.0) ?= s"+${math.round(100 * bonus).toInt}c"
  //         ),
  //         <.input(
  //           isNotAssigned ?= (^.disabled := true),
  //           ^.float := "left",
  //           ^.`type` := "text",
  //           ^.placeholder := s"Question",
  //           ^.margin := "1px",
  //           ^.marginLeft := "26px",
  //           ^.padding := "1px",
  //           ^.width := "240px",
  //           ^.onChange ==> (
  //             (e: ReactEventI) => {
  //               val newValue = e.target.value
  //               scope.modState(
  //                 qaPairsLens.modify(
  //                   qaPairs => {
  //                     val currentQA = qaPairs(index)
  //                     val newQA = currentQA.copy(_1 = newValue)
  //                     qaPairs.updated(index, newQA)
  //                   }))
  //             }),
  //           ^.onFocus --> scope.modState(currentFocusLens.set(index)),
  //           ^.value := qaPairs(index)._1
  //         ),
  //         <.div(
  //           Styles.answerIndicator,
  //           ^.float := "left",
  //           ^.minHeight := "1px",
  //           ^.width := "25px",
  //           isFocused ?= "-->"
  //         ),
  //         <.div(
  //           ^.float := "left",
  //           ^.margin := "1px",
  //           ^.padding := "1px",
  //           isAnswerEmpty ?= (^.color := "#CCCCCC"),
  //           if(isAnswerEmpty && isFocused) {
  //             "Highlight your answer above"
  //           } else {
  //             TextRendering.renderSentence(sentence.words.filter(w => qaPairs(index)._2.contains(w.index)).map(_.token))
  //           }
  //         )
  //       )
  //   }

  //   def touchWord(index: Int): Callback = scope.modState {
  //     case s @ Loading(_) => s
  //     case s @ Loaded(sentence, qaPairs, currentFocus, highlightingState) =>
  //       if(isNotAssigned) {
  //         s
  //       } else {
  //         val answerSpans = qaPairs.map(_._2)
  //         val curAnswer = answerSpans(currentFocus)
  //         highlightingState match {
  //           case DoingNothing => s
  //           case Highlighting =>
  //             if(!curAnswer.contains(index)) {
  //               qaPairsLens.modify(
  //                 qaPairs => {
  //                   val currentQA = qaPairs(currentFocus)
  //                   val newQA = currentQA.copy(_2 = currentQA._2 + index)
  //                   qaPairs.updated(currentFocus, newQA)
  //                 })(s)
  //             } else s
  //           case Erasing =>
  //             if(curAnswer.contains(index)) {
  //               qaPairsLens.modify(
  //                 qaPairs => {
  //                   val currentQA = qaPairs(currentFocus)
  //                   val newQA = currentQA.copy(_2 = currentQA._2 - index)
  //                   qaPairs.updated(currentFocus, newQA)
  //                 })(s)
  //             } else s
  //         }
  //       }
  //   }

  //   def render(s: State) = {
  //     <.div(
  //       ^.onMouseUp --> scope.modState(highlightingStateLens.set(DoingNothing)),
  //       ^.onMouseDown --> scope.modState(highlightingStateLens.set(Highlighting)),
  //       Styles.mainContent,
  //       instructions,
  //       <.hr(),
  //       s match {
  //         case Loading(msg) =>
  //           <.p(s"Loading sentence ($msg)...")
  //         case ls @ Loaded(sentence, qaPairs, currentFocus, highlightingState) =>
  //           val answerSpans = qaPairs.map(_._2)
  //           val curAnswer = answerSpans(currentFocus)
  //           val otherAnswerWords = (answerSpans.take(currentFocus) ++ answerSpans.drop(currentFocus + 1)).flatten.toSet
  //           import scalaz.std.list._
  //           <.div(
  //             <.p(
  //               Styles.unselectable,
  //               TextRendering.renderSentence(
  //                 sentence.words,
  //                 (word: CoNLLWord) => word.token,
  //                 (nextWord: CoNLLWord) => List(
  //                   <.span(
  //                     ^.backgroundColor := (
  //                       if(curAnswer.contains(nextWord.index) && curAnswer.contains(nextWord.index - 1)) {
  //                         "#FFFF00"
  //                       } else if(otherAnswerWords.contains(nextWord.index) &&
  //                                   otherAnswerWords.contains(nextWord.index - 1)) {
  //                         "#CCCCCC"
  //                       } else {
  //                         "transparent"
  //                       }),
  //                     " ")),
  //                 (word: CoNLLWord) => List(
  //                   <.span(
  //                     word.index == prompt.wordIndex ?= Styles.specialWord,
  //                     ^.backgroundColor := (
  //                       if(curAnswer.contains(word.index)) {
  //                         "#FFFF00"
  //                       } else if(otherAnswerWords.contains(word.index)) {
  //                         "#CCCCCC"
  //                       } else {
  //                         "transparent"
  //                       }
  //                     ),
  //                     ^.onMouseMove --> touchWord(word.index),
  //                     ^.onMouseDown ==> (
  //                       (e: ReactEventI) => if(curAnswer.contains(word.index)) {
  //                         e.stopPropagation
  //                         scope.modState(highlightingStateLens.set(Erasing)) >> touchWord(word.index)
  //                       } else {
  //                         scope.modState(highlightingStateLens.set(Highlighting)) >> touchWord(word.index)
  //                       }
  //                     ),
  //                     TextRendering.normalizeToken(word.token)
  //                   ))
  //               )),
  //             <.ul(
  //               Styles.listlessList,
  //               (0 until qaPairs.size).map(i =>
  //                 <.li(
  //                   ^.display := "block",
  //                   qaField(ls, i, bonuses(i))
  //                 )
  //               )
  //             )
  //           )
  //       }
  //     )
  //   }
  // }

  // val FullUI = ReactComponentB[Unit]("Full UI")
  //   .initialState(Loading("Connecting to server"): State)
  //   .renderBackend[FullUIBackend]
  //   .componentDidMount(context => context.backend.load)
  //   .componentDidUpdate(context => context.$.backend.updateResponse)
  //   .build

  def main(): Unit = jQuery { () =>
    // Styles.addToDocument()
    // ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  // private[this] val instructions = <.div(
  //   <.h2("""Task Summary"""),
  //   <.p(<.span("""This task is for an academic research project by the natural language processing group at the University of Washington.
  //       We wish to deconstruct the meanings of English sentences into a list of questions and answers.
  //       You will be presented with a selection of English text with a """), <.b("special word"), " written in bold."),
  //   <.p("""You will write questions and their answers, where the answer is taken from the sentence and """,
  //       <.b("""either the question or the answer contains the special word. """),
  //       """You will earn bonuses by writing more questions and answers.
  //       For example, consider the sentence:"""),
  //   <.blockquote(<.i("The jubilant ", <.span(Styles.specialWord, "protesters"),
  //                    " celebrated after executive intervention canceled the project.")),
  //   <.p("""Valid question-answer pairs include:"""),
  //   <.ul(
  //     <.li("How did the ", <.b("protesters "), "feel? --> jubilant"),
  //     <.li("Who celebrated? --> the ", <.b("protesters"))),
  //   <.h2("""Requirements"""),
  //   <.p("""This task is best fit for native speakers of English.
  //       Your response must be grammatical, fluent English that satisfies the following criteria:"""),
  //   <.ol(
  //     <.li("""Either the question or the answer contains the special word."""),
  //     <.li("""The question contains at least one word from the sentence, but as few as possible beyond that."""),
  //     <.li("""The answer is the longest natural, correct, and unique answer to the question."""),
  //     <.li("""You do not repeat the same information between multiple question-answer pairs.""")
  //   ),
  //   <.p("See the examples for further explanation."),
  //   <.h2("""Good Examples"""),
  //   <.p("Suppose you are given the following sentence:"),
  //   <.blockquote(<.i("""I take full and complete responsibility for my thoughtless """, <.span(Styles.specialWord, """decision"""),
  //                    """ to disclose these materials to the public. """)),
  //   <.p("""Acceptable questions and answers include, but are not limited to, the following.
  //       Mouse over each example to see an explanation."""),
  //   <.ul(
  //     <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
  //                <.span("Who "), <.b("decided "), <.span("something? --> I"),
  //                <.span(^.className := "tooltiptext",
  //                       """It is often useful to change nouns like "decision" to verbs in order to use them in short questions."""))),
  //     <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
  //                <.span("What is someone responsible for? --> my thoughtless "), <.b("decision"),
  //                <.span(" to disclose these materials to the public"),
  //                <.span(^.className := "tooltiptext",
  //                       "Prefer the longest answer that correctly and naturally answers the question."))),
  //     <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
  //          <.span("What kind of "), <.b("decision"), <.span("? --> thoughtless"),
  //          <.span(^.className := "tooltiptext",
  //                 """To get descriptive words as answers, you may need to ask "What kind" or similar questions."""))),
  //     <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
  //          <.span("What did someone "), <.b("decide "), <.span("to do? --> disclose these materials to the public"),
  //          <.span(^.className := "tooltiptext",
  //                 """Use words like "do", "someone" and "something" in your question
  //                    to avoid using more than one word from the sentence.""")))
  //   ),
  //   <.h2("""Bad Examples"""),
  //   <.p("Suppose you are given the following sentence:"),
  //   <.blockquote(<.i("""Alex """, <.span(Styles.specialWord, "pushed"), """ Chandler at school today.""")),
  //   <.p("""Consider the following question-answer pairs:"""),
  //   <.ul(
  //     <.li(<.div("What did Alex do? --> ", <.span(Styles.specialWord, "pushed"), " Chandler at school today")),
  //     <.li(<.div("Who ", <.span(Styles.specialWord, "pushed"), " someone? --> Alex"))
  //   ),
  //   <.p("These are each acceptable on their own, but ", <.b("you may not provide both, "),
  //       """because they convey the same information, just reversing the order of the question and answer.
  //       One way to notice this is that the answer to one question ("Alex") appears in the other question."""),
  //   <.ul(
  //     <.li(<.div("When did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today")),
  //     <.li(<.div("On what day did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today"))
  //   ),
  //   <.p("""Here, the second is just a minor rephrasing of the first, which is not acceptable.
  //       One way to notice this is that both questions have the same answer."""),
  //   <.p("Mouse over the following examples of ", <.b("bad"), " question-answer pairs for explanations:"),
  //   <.ul(
  //     <.li(<.div(Styles.badRed, ^.className := "tooltip",
  //                <.span("When did Alex "), <.b("push"), <.span(" Chandler? --> today"),
  //                <.span(^.className := "tooltiptext",
  //                       """This is bad because the question includes "Alex" and "Chandler" where it should have said "someone"
  //                       to use fewer words from the sentence."""))),
  //     <.li(<.div(Styles.badRed, ^.className := "tooltip",
  //                <.span("Where did Alex do something? --> at school"),
  //                <.span(^.className := "tooltiptext",
  //                       """This is bad because it fails to include the special word "push" in either the question or answer."""))),
  //     <.li(<.div(Styles.badRed, ^.className := "tooltip",
  //                <.span("Where did someone "), <.b("push "), <.span("someone? --> at school today"),
  //                <.span(^.className := "tooltiptext",
  //                       """This is bad because the question asked "where", so including the word "today" is incorrect.""")))
  //   ),
  //   <.h2("""Conditions & Bonuses"""),
  //   <.p("""If your work satisfies the criteria outlined here, it will be approved in at most one hour.
  //         If it repeatedly fails to meet requirements, you will be blocked from this task and future tasks.
  //         Mosts HITs should go quickly (less than 30 seconds) since there will often be
  //         only one good question-answer pair to write."""),
  //   <.p(s"""For each HIT, the first question-answer pair is required.
  //       After that you will receive progressively increasing bonuses if you can come up with more.
  //       The order in which you fill out the fields does not matter, but the bonus amounts
  //       are written next to each response field to help you keep track.
  //     """),
  //   <.p("""If you have any questions, concerns, or points of confusion,
  //       please share them in the "Feedback" field so we may improve the task.""")
  //   )
}
