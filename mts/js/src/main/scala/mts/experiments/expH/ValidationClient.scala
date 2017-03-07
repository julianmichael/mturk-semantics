package mts.experiments.expH

import mts.experiments._
import mts.conll._
import mts.util.dollarsToCents
import mts.tasks._
import mts.language._
import mts.experiments.expF.WebsocketLoadableComponent

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

object ValidationClient extends TaskClient[ValidationPrompt, List[ValidationAnswer]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[ValidationApiRequest, ValidationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int)]
  import HighlightingComponent._

  lazy val questions = prompt.qaPairs.map(_.question)

  @Lenses case class State(
    curQuestion: Int,
    isInterfaceFocused: Boolean,
    answers: List[ValidationAnswer])
  object State {
    def initial = State(0, false, questions.map(_ => Answer(Set.empty[Int])))
  }

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse: Callback = scope.state.map { state =>
      setSubmitEnabled(state.answers.forall(_.isComplete))
      setResponse(state.answers)
    }

    def updateHighlights(hs: HighlightingState) = {
      val span = hs.span
      scope.modState(
        State.answers.modify(answers =>
          answers.zipWithIndex.map {
            case (Answer(_), i) => Answer(span.filter(_._1 == i).map(_._2))
            case (invalidOrRedundant, _) => invalidOrRedundant
          }
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

    def qaField(s: State, sentence: Vector[String], span: Set[(Int, Int)])(index: Int) = {
      val isFocused = s.curQuestion == index
      val answer = s.answers(index)
      def highlightedSpanFor(i: Int) = Answer(span.filter(_._1 == i).map(_._2))

      <.div(
        ^.overflow := "hidden",
        <.div(
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "50px",
          answer.isInvalid ?= (^.backgroundColor := "#E01010"),
          ^.onClick --> scope.modState(
            State.answers.modify(answers =>
              answers.updated(
                index,
                if(answers(index).isInvalid) highlightedSpanFor(index)
                else InvalidQuestion)
            )
          ),
          "Invalid"
        ),
        <.span(
          isFocused ?= Styles.bolded,
          s.answers(s.curQuestion).getRedundant.fold(false)(_.other == index) ?= Styles.uncomfortableOrange,
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(s =>
            if(s.curQuestion == index || s.answers(index).isRedundant) s
            else if(s.answers(s.curQuestion).getRedundant.fold(false)(_.other == index)) {
              State.answers.modify(answers => answers.updated(s.curQuestion, highlightedSpanFor(s.curQuestion)))(s)
            } else {
              State.answers.modify(answers => answers.updated(s.curQuestion, Redundant(index)))(s)
            }
          ),
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
          (answer.getAnswer.fold(true)(_.indices.isEmpty)) ?= (^.color := "#CCCCCC"),
          answer match {
            case InvalidQuestion => "N/A"
            case Redundant(other) => <.span("Redundant with ", <.i(questions(other)))
            case Answer(span) if span.isEmpty && isFocused =>
              "Highlight answer above, move with arrow keys, or click on a redundant question"
            case Answer(span) => TextRendering.renderSentence(
              sentence.zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
          }
        )
      )
    }

    def render(state: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri, request = ValidationApiRequest(prompt.path), render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(ValidationApiResponse(sentence), _) =>
              import scalaz.std.list._
              import state._
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned && answers(curQuestion).isAnswer, update = updateHighlights, render = {
                    case (hs @ HighlightingState(spans, status), HighlightingContext(_, startHighlight, startErase, stopHighlight, touchElement)) =>
                      val showHighlights = answers(curQuestion).isAnswer
                      val curSpan = spans.collect { case (`curQuestion`, i) => i }
                      def touchWord(i: Int) = touchElement((curQuestion, i))
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
                          !isInterfaceFocused ?= <.div(
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
                              sentence.indices,
                              getToken = (index: Int) => sentence(index),
                              spaceFromNextWord = (nextIndex: Int) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(showHighlights && curSpan.contains(nextIndex) && curSpan.contains(nextIndex - 1)) {
                                      "#FFFF00"
                                    } else "transparent"),
                                  " ")),
                              renderWord = (index: Int) => List(
                                <.span(
                                  ^.backgroundColor := (
                                    if(showHighlights && curSpan.contains(index)) "#FFFF00"
                                    else "transparent"),
                                  ^.onMouseMove --> touchWord(index),
                                  ^.onMouseDown ==> (
                                    (e: ReactEventI) => if(curSpan.contains(index)) {
                                      e.stopPropagation // so we don't trigger the global startHighlight
                                      startErase >> touchWord(index)
                                    } else {
                                      startHighlight >> touchWord(index)
                                    }
                                  ),
                                  TextRendering.normalizeToken(sentence(index))
                                ))
                            )),
                          <.ul(
                            Styles.listlessList,
                            (0 until questions.size)
                              .map(qaField(state, sentence, hs.span))
                              .map(field => <.li(^.display := "block", field))
                          ),
                          <.p(
                            s"Bonus: ${dollarsToCents(validationBonusPerQuestion * math.max(0, questions.size - 4))}c"
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
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

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
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into a list of questions and answers.
           You will be presented with a selection of English text and a list of questions (usually at least four)
           prepared by other annotators."""),
    <.p("""You will highlight the words in the sentence that correctly answer the question,
           as well as mark whether questions are invalid or redundant.
           For example, for the following sentence and questions, you should respond with the answers in green:"""),
    <.blockquote(<.i("The jubilant protesters celebrated after executive intervention canceled the project.")),
    <.ul(
      <.li("How did the protesters feel? --> ", <.span(Styles.goodGreen, "jubilant")),
      <.li("When did someone celebrate? --> ", <.span(Styles.goodGreen, "after executive intervention canceled the project")),
      <.li("Who celebrated? --> ", <.span(Styles.goodGreen, "The jubilant protesters"))),
    <.p(s"""You will be paid in accordance with the number of questions shown to you, with a bonus of
            ${dollarsToCents(validationBonusPerQuestion)}c per question after the first four
            that will be paid when the assignment is approved."""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Each of your answers must satisfy the following criteria:"""),
    <.ol(
      <.li("""It is a correct answer to the question."""),
      <.li("""It is grammatical, to the extent possible.""")),
    <.p("""If there are multiple correct answers, include all of them in your answer if possible.
        (Skipping over words might help with this.) """),
    <.p("""Another part of your job is to identify which questions are """, <.b("invalid"), """.
        Instead of providing an answer, you should mark a question as invalid if it satisfies any of the following criteria:"""),
    <.ul(
      <.li("The question has grammatical or spelling errors or is not fluent English."),
      <.li("The question is not answered explicitly in the sentence."),
      <.li("The question does not contain any words taken from sentence."),
      <.li("It is a yes/no question, an either/or question, or some other non-open-ended question.")
    ),
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
      example(question = "Who leaked the documents?",
              answer = "I",
              isGood = false,
              tooltip = """This question does not contain any content words from the sentence, making it invalid."""),
      example(question = "Is it full responsibility or not?",
              answer = "full and complete",
              isGood = false,
              tooltip = """Yes/no questions and either/or questions are forbidden; this question should be marked invalid.""")
    ),
    <.h2("Redundancy"),
    <.p("""Some questions are not invalid, but you should mark them as redundant with others.
        To clarify what this means, suppose you are given the following sentence:"""),
    <.blockquote(<.i("""Alex pushed Chandler at school today.""")),
    <.p("""Consider the following questions:"""),
    <.ul(
      <.li("When did someone push someone?"),
      <.li("On what day did someone push someone?")
    ),
    <.p("""The second is just a minor rephrasing of the first, which is not acceptable.
        One way to notice this is that both questions have the same answer.
        However, consider the following three question-answer pairs:"""),
    <.ol(
      <.li("Where did someone push someone?"),
      <.li("Where did someone push someone today?"),
      <.li("Where was Alex today?")
    ),
    <.p("""Number 2 is redundant with number 1,
        because they are asking the same question but with different amounts of detail.
        Number 3, however, is not redundant, because it is asking about the location of Alex and not the pushing.
        Finally, consider the following:"""),
    <.ul(
      <.li("What did Alex do?"),
      <.li("Who pushed Chandler?")
    ),
    <.p("""These are redundant with each other because they convey the same information,
        but just reverse the order of the question and answer.
        One way to notice this is that the answer to one question appears in the other, and vice versa."""),
    <.p("""Sometimes you may not be sure whether to include some words in your answer.
        As a rule of thumb, include only what is necessary for the answer to be completely specified,
        but if all else is equal, prefer longer answers over shorter ones."""),
    <.h3("Note about the Interface"),
    <.p("""To mark a question as redundant, while that question is selected,
           you will click on the question it's redundant with (which will turn orange).
           For each group of redundant questions, you should provide an answer for exactly one of them,
           and mark the rest of them redundant with that one.
           To un-mark a question as redundant, just click on the orange sentence again."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field so we may improve the task."""),
    <.h2("Conditions and payment"),
    <.p("""Your answers and validity judgments will be cross-checked with other workers.
        If your agreement with other annotators drops too low or we detect you spamming,
        you will be warned, and then you will be blocked from this task and future tasks
        if you continue without improving.
        Otherwise, your work will be approved and the bonus will be paid within an hour.""")
  )
}
