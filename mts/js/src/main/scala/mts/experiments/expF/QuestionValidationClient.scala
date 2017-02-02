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

object QuestionValidationClient extends TaskClient[ValidationPrompt, QuestionValidationResponse] {

  lazy val isNotAssigned = assignmentId == "ASSIGNMENT_ID_NOT_AVAILABLE"

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[ApiRequest, ApiResponse]
  import WebsocketLoadableComponent._

  @Lenses case class State(
    curQuestion: Int,
    questions: List[Option[String]])
  object State {
    def initial = State(0, prompt.sourcedQAPairs.map(qa => Some(qa.question)))
  }

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse: Callback = scope.state.map { st =>
      setSubmitEnabled(true)
      setResponse(QuestionValidationResponse(st.questions))
    }

    def qaField(state: State, sentence: CoNLLSentence)(index: Int) = state match {
      case State(curQuestion, questions) =>
        val isFocused = curQuestion == index
        val answerIndices = prompt.sourcedQAPairs(index).answer
        val originalQuestion = prompt.sourcedQAPairs(index).question
        val answer = TextRendering.renderSentence(
          sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))

        <.div(
          ^.overflow := "hidden",
          <.input(
            ^.float := "left",
            ^.`type` := "button",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "50px",
            ^.onClick --> scope.modState(State.questions.modify(_.updated(index, Some(originalQuestion)))),
            ^.value := "Reset"
          ),
          <.input(
            ^.float := "left",
            ^.`type` := "button",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "50px",
            state.questions(index).isEmpty ?= (^.backgroundColor := "#E01010"),
            ^.onClick --> scope.modState(
              State.questions.modify(
                _.updated(
                  index,
                  questions(index).fold(Option(originalQuestion))(_ => None)
                )
              )
            ),
            ^.value := "Invalid"
          ),
          <.input(
            isNotAssigned ?= (^.disabled := true),
            ^.float := "left",
            ^.`type` := "text",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "360px",
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = Some(e.target.value)
                scope.modState(State.questions.modify(_.updated(index, newValue)))
              }),
            ^.onFocus --> scope.modState(State.curQuestion.set(index)))(
            (questions(index) match {
               case None => List[TagMod](
                 ^.backgroundColor := "#CCCCCC",
                 ^.disabled := true,
                 ^.value := prompt.sourcedQAPairs(index).question
               )
               case Some(q) => List[TagMod](^.value := q)
             }): _*),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            answer
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
              val curSpan = prompt.sourcedQAPairs(s.curQuestion).answer
                <.div(
                  Styles.mainContent,
                  instructions,
                  <.hr(),
                  <.div(
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
                            TextRendering.normalizeToken(word.token)
                          ))
                      )),
                    <.ul(
                      Styles.listlessList,
                      (0 until s.questions.size)
                        .map(qaField(s, sentence))
                        .map(field => <.li(^.display := "block", field))
                    )
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
    .componentDidUpdate(_.$.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  val exDivStyle = List(^.margin := "10px", ^.width := "360px", ^.float := "left")
  def example(origQ: String, answer: String, newQ: Option[String], isGood: Boolean, tooltip: String) =
    <.li(
      ^.overflow := "hidden",
      ^.borderBottom := "1px solid",
      <.div(exDivStyle)(
        <.div(
          ^.position := "relative",
          <.b("Original")
        ),
        <.div(
          ^.position := "relative",
          <.span(<.b("Q: "), origQ)
        ),
        <.div(
          ^.position := "relative",
          <.span(<.b("A: "), answer)
        )
      ),
      <.div(exDivStyle)(
        ^.width := "250px",
        <.div(
          ^.position := "relative",
          <.b("Revised")
        ),
        <.div(
          ^.position := "relative",
          isGood ?= Styles.goodGreen,
          !isGood ?= Styles.badRed,
          newQ.fold(<.span("<Invalid>"))(q => <.span(<.b("Q: "), q))
        )
      ),
      <.div(exDivStyle)(
        <.div(
          ^.position := "relative",
          <.b("Explanation")
        ),
        <.div(
          ^.position := "relative",
          tooltip
        )
      )
    )

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of English sentences into a list of questions and answers.
        You will be presented with a selection of English text and a list of question-answer pairs about that selection
        prepared by other annotators."""),
    <.p("""You will revise and the questions to make them shorter and remove unneeded content words
        while preserving their meaning. Consider the following example:"""),
    <.blockquote(<.i("The jubilant protesters celebrated after executive intervention canceled the project.")),
    <.ul(
      <.li("How did the celebrating protesters feel? --> jubilant"),
      <.li("What did the protesters celebrate after? --> executive intervention")),
    <.p("""You will revise the first question to """, <.b("How did the protesters feel? "),
        """and the second question to """, <.b("What did someone celebrate after?")),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        For each question, your revised version must be fluent, grammatical English and satisfy the following criteria:"""),
    <.ol(
      <.li("""Any typos, spelling errors, or grammatical errors have been fixed."""),
      <.li("""The question asks about the same thing, and has the same correct answer as before."""),
      <.li("""Any unnecessary words from the sentence have been removed or replaced with "someone," "something," or other generic words."""),
      <.li("""The question still contains at least one content word from the sentence.""")),
    <.p("""If the question is incoherent, has no apparent connection to the answer,
        has multiple correct answers, or is otherwise unsalvageable,
        you should mark it """, <.b("Invalid"), """. See the examples for further explanation."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i("""The new chairman fears a large decrease in profits in the second quarter,
            after shaky performance in the beginning of the year.""")),
    <.p("""Here are examples of revisions you might make:"""),
    <.ul(
      Styles.listlessList,
      example(origQ = "What is feared to decrease?", answer = "profits",
              newQ = Some("What may decrease?"), isGood = true,
              tooltip = """Try to eliminate all but one word from the sentence if possible.
                           Feel free to change the phrasing to make the question more natural."""),
      example(origQ = """Shaky performance resulted in an expected decrease in what?""", answer = "profits",
              newQ = Some("A decrease in what?"), isGood = true,
              tooltip = """The question does not need to be a complete sentence. Keep only the words that are necessary,
                           but make sure it is fluent English."""),
      example(origQ = "What direction will profits go?", answer = "decrease",
              newQ = Some("What might profits do?"), isGood = true,
              tooltip = """This question was almost unsalvageable, because "decrease" is not a proper answer.
                           It also would be acceptable to mark this question Invalid."""),
      example(origQ = "what does the chairman ffear", answer = "a decrease in profits",
              newQ = Some("What does someone fear?"), isGood = true,
              tooltip = """Make sure to replace words and phrases with "someone" or "something" if you can.
                           Also correct typos, capitalization, and punctuation if necessary."""),
      example(origQ = "What is feared to happen to profits?", answer = "a larger decrease",
              newQ = Some("What may happen to profits?"), isGood = true,
              tooltip = """If there are extra verbs in the sentence, make sure to remove them as well;
                           this can often be done with words like "may" and "might"."""),
      example(origQ = "Which quarter?", answer = "second",
              newQ = Some("Which quarter?"), isGood = true,
              tooltip = """If the question is fine as-is, don't change it.""")
    ),
    <.p("""Your revisions will be examined and cross-checked with other workers.
           If you spam or consistently provide low-quality revisions, you will be banned this task and future tasks.
           Otherwise, your work will be approved within an hour.
        """),
    <.p("""If you have any questions, concerns, or points of confusion,
           please share them in the "Feedback" field so we may improve the task.""")
  )
}
