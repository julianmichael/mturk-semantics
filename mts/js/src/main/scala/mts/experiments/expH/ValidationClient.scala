package mts.experiments.expH

import mts.experiments._
import mts.datasets.conll._
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
            if(s.curQuestion == index || !s.answers(index).isAnswer) s
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
          websocketURI = websocketUri, request = ValidationApiRequest(prompt.id), render = {
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
                          <.p(s"Bonus: ${dollarsToCents(validationBonus(questions.size))}c")
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
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions (usually at least four)
           prepared by other annotators."""),
    <.p(<.b("Note: "), """While there may be few HITs available at any one time, more will be continuously uploaded
           as other workers write questions for you to validate."""),
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
        For each question, you will either """,
        <.b("answer it, "), "mark it ", <.b("invalid, "), "or mark it ", <.b("redundant.")),
    <.h3("Answers"),
    <.p("""Each answer must be """, <.b("correct "), "and ", <.b("as grammatical as possible"),
        """. If there are multiple correct answers, include all of them in your answer if possible.
        Include only the words necessary to completely answer the question,
        but if all else is equal, prefer longer answers over shorter ones."""),
    <.h3("Invalid Questions"),
    <.p("""A question should be marked invalid if any of the following are true:"""),
    <.ul(
      <.li("""It isn't about the meaning of the sentence (for example, asking "Which word comes after...")."""),
      <.li("It has grammatical or spelling errors."),
      <.li("It is not obviously and explicitly answered in the sentence."),
      <.li("""It does not contain any words from the sentence (for example, "What happened?" is usually invalid)."""),
      <.li("It is a yes/no or either/or question, or other non-open-ended question.")
    ),
    <.p("""It is okay for a question not to be a full sentence, as long as it makes sense and it is grammatical English.
           For example, the question "Whose decision?" would be fine if the phrase "my decision" appeared in the sentence."""),
    <.p("""If the question is a bit odd and you're not sure how to mark it, it is probably invalid.
           Good questions should be obviously good."""),
    <.h3("Redundancy"),
    <.p(""" Two questions are """, <.b("redundant "), """if they """,
        <.b("have the same meaning "), "and they ", <.b("have the same answer. "), """
        If any question is redundant with another, you should mark it as such.
        For example, suppose you are given the following sentence and questions:"""),
    <.blockquote(<.i("""Intelligence documents leaked to the public today have dealt another blow to the agency's credibility.""")),
    <.ul(
      <.li(<.div("When was something leaked?")),
      <.li(<.div("On what day was something leaked?"))
    ),
    <.p("""They have the same answer (""", <.i("today"), """) and the second question is just a minor rephrasing of the first, so """,
        <.b(Styles.badRed, "these are redundant. "), """
        However, consider the following:"""),
    <.ul(
      <.li(<.div("What was leaked today?")),
      <.li(<.div("What kind of documents?"))
    ),
    <.p("""While these both may be answered with the same phrase, """, <.i("intelligence documents"), """,
        these questions are """, <.b(Styles.goodGreen, "not redundant "), """ because they are asking about different things:
        the first is asking about what it is that leaked,
        and the second is asking about a characteristic of the documents."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i(""" In the year since the regulations were enacted,
                         the Director of the Environmental Protection Agency (EPA),
                         Gina McCarthy, has been aggressive in enforcing them.""")),
    <.p("""Here are examples of questions others might write, and how you should answer them.
           Mouse over each for an explanation."""),
    <.ul(
      example(question = "What was enacted?", answer = "the regulations", isGood = true,
              tooltip = """This is a standard, straightforward question that is answered literally by the sentence.
                           Most questions should look something like this."""),
      example(question = "In the what since?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer simply replaced a word with "what"
                           instead of using it to form a proper English question."""),
      example(question = "How long was it since the regulations were enacted?", answer = "the year", isGood = true,
              tooltip = """While "a year" is a more natural answer, "the year" is the closest you can get
                           and the question is answered in the sentence so it is still acceptable."""),
      example(question = "What does EPA stand for?", answer = "Environmental Protection Agency", isGood = true,
              tooltip = """Asking about the meanings of words or acronyms, when they are explicitly defined
                           in the sentence, is acceptable."""),
      example(question = "What pronoun refers to the regulations?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer may have had the word "them" in mind, but this question
                           is about the words in the sentence instead of the sentence's meaning,
                           so it is unacceptable."""),
      example(question = "Who enacted the regulations?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer may have been thinking it was the EPA, but that is not
                           stated explicitly in the sentence, so the question is invalid.
                           (In fact, they were also wrong: it is Congress which enacts regulations, and not the EPA.)"""),
      example(question = "What is Gina's last name?", answer = "McCarthy", isGood = true,
              tooltip = """This is an acceptable question much like "What does EPA stand for?" """),
      example(question = "Was McCarthy aggressive or lax?", answer = "<Invalid>", isGood = false,
              tooltip = """This is an either/or question, which is disallowed.""")
    ),
    <.p("Now suppose you are given the following sentence:"),
    <.blockquote(<.i(""" "I take full and complete responsibility," she said, "for
                        my decision to disclose these materials to the public." """)),
    <.p("""Here are some more examples:"""),
    <.ul(
      example(question = "Who decided to disclose something?", answer = "I", isGood = true,
              tooltip = """To deal with pronouns like "I", you should choose answers from the perspective of the speaker."""),
      example(question = "What is someone responsible for?", answer = "my decision to disclose these materials to the public", isGood = true,
              tooltip = """If shorter and longer answers both suffice, favor the longer one.
                           Provide this answer instead of just "my decision"."""),
      example(question = "Who made the decision?",
              answer = """<Redundant with "Who decided to disclose something?">""", isGood = false,
              tooltip = """The question has the same meaning as asking who "decided" to do it,
                           as in the first question---and the answer, "I", is the same,
                           so this question is redundant."""),
      example(question = "What did someone take?",
              answer = "responsibility for my decision to disclose these materials to the public",
              isGood = true,
              tooltip = """Skip over words if necessary to produce a complete and grammatical answer."""),
      example(question = "Who leaked the documents?",
              answer = "<Invalid>",
              isGood = false,
              tooltip = """This question does not contain any content words from the sentence.""")
    ),
    <.h2("Interface Controls"),
    <.ul(
      <.li("Change questions using the arrow keys."),
      <.li("Highlight your answer in the sentence."),
      <.li("""To mark the selected question as redundant, just click the question it's redundant with (which will turn orange).
              To unmark it, click the orange question again."""),
      <.li("""You can only mark a question as redundant with a question that has an answer.
              (If more than two questions are mutually redundant, answer one of them and mark the others as redundant with that one.)""")
    ),
    <.h2("Conditions and payment"),
    <.p(s"""Your judgments will be cross-checked with other workers.
        If your agreement with other annotators drops below
        ${(validationAgreementThreshold * 100).toInt} percent,
        you will be warned, and then you will be blocked from this task and future tasks
        if you continue without improving.
        If we find you spamming, your work will be rejected and you will be blocked without warning.
        Otherwise, your work will be approved and the bonus will be paid within an hour."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field so we may improve the task.""")
  )
}
