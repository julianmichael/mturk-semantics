package mts.experiments.expG

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

import scalaz.std.list._

object WordLimitingQuestionValidationClient extends TaskClient[TokenizedValidationPrompt, KeywordQuestionValidationResponse] {

  val WebsocketLoadableComponent = new expF.WebsocketLoadableComponent[ApiRequest, ApiResponse]
  import WebsocketLoadableComponent._

  val numQAPairs = prompt.sourcedTokenizedQAPairs.size

  @Lenses case class State(
    curQuestion: Int,
    keywordPicking: Option[Set[Int]],
    validatedQuestions: List[Option[ValidatedQuestion]]) {
    def keywordFor(i: Int): Option[Int] = validatedQuestions(i)
      .flatMap(_.fold(None: Option[Int])((j: Int, _: String) => Some(j)))
    def curKeyword: Option[Int] = keywordFor(curQuestion)
  }
  object State {
    def initial = State(0, None, List.fill(numQAPairs)(None: Option[ValidatedQuestion]))
  }

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def updateResponse: Callback = scope.state.map { st =>
      import scalaz._
      import Scalaz._
      st.validatedQuestions.sequence match {
        case None => setSubmitEnabled(false)
        case Some(vqs) =>
          setSubmitEnabled(true)
          setResponse(KeywordQuestionValidationResponse(vqs))
      }
    }

    def qaField(state: State, sentence: CoNLLSentence, alignedTokens: Set[(String, Int)])(index: Int) = state match {
      case State(curQuestion, _, validatedQuestions) =>
        val isFocused = curQuestion == index
        val answerIndices = prompt.sourcedTokenizedQAPairs(index).answer
        val originalQuestionTokens = prompt.sourcedTokenizedQAPairs(index).questionTokens
        val answer = TextRendering.renderSentence(
          sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))

        val tryingToPickKeyword = isFocused && !state.keywordPicking.isEmpty

        <.div(
          ^.overflow := "hidden",
          <.div(
            isFocused ?= (if(tryingToPickKeyword) Styles.badRedBold else Styles.niceBlue),
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "100px",
            ^.minHeight := "1px",
            state.keywordFor(index).fold(if(tryingToPickKeyword) "?" else "")(kwIndex =>
              TextRendering.normalizeToken(sentence.words(kwIndex).token)
            )
          ),
          <.input(
            isNotAssigned ?= (^.disabled := true),
            ^.float := "left",
            ^.`type` := "button",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "50px",
            ^.onClick --> scope.modState(State.validatedQuestions.modify(_.updated(index, None))),
            ^.value := "Reset"
          ),
          <.input(
            isNotAssigned ?= (^.disabled := true),
            ^.float := "left",
            ^.`type` := "button",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "50px",
            validatedQuestions(index).fold(false)(_ == InvalidQuestion) ?= (^.backgroundColor := "#E01010"),
            ^.onClick --> scope.modState(
              State.validatedQuestions.modify(
                _.updated(
                  index,
                  validatedQuestions(index).fold(Option(InvalidQuestion: ValidatedQuestion)) {
                    case InvalidQuestion => None
                    case EditedQuestion(_, _) => Some(InvalidQuestion)
                  })
              )),
            ^.value := "Invalid"
          ),
          validatedQuestions(index) match {
            case None => <.div(
              ^.float := "left",
              ^.width := "320px",
              ^.margin := "1px",
              ^.padding := "1px",
              TextRendering.renderSentence(
                originalQuestionTokens,
                getToken = identity[String],
                spaceFromNextWord = (_: String) => List(<.span(" ")),
                renderWord = (token: String) => List(
                  <.span(
                    Styles.hoverBlueBold,
                    ^.onClick --> (
                      if(isNotAssigned) {
                        Callback.empty
                      } else {
                        val tokenIndices = alignedTokens.filter(_._1.equals(token.toLowerCase)).map(_._2)
                        if(tokenIndices.size == 1) {
                          scope.modState(
                            State.keywordPicking.set(None) andThen State.validatedQuestions.modify(
                              _.updated(
                                index,
                                Some(EditedQuestion(tokenIndices.head, TextRendering.renderSentence(originalQuestionTokens))))
                            ),
                            Callback(scala.scalajs.js.Dynamic.global.document.getElementById(s"qa-$index").focus())
                          )
                        } else {
                          scope.modState(
                            State.curQuestion.set(index) andThen State.keywordPicking.set(Some(tokenIndices))
                          )
                        }
                      }
                    ),
                    token
                  )
                )
              )
            )
            case Some(InvalidQuestion) => <.div(
              ^.float := "left",
              ^.margin := "1px",
              ^.padding := "1px",
              ^.width := "320px",
              ^.color := "#CCCCCC",
              TextRendering.renderSentence(originalQuestionTokens)
            )
            case Some(EditedQuestion(keywordIndex, newString)) => <.input(
              ^.`type` := "text",
              ^.id := s"qa-$index",
              ^.float := "left",
              ^.margin := "1px",
              ^.padding := "1px",
              ^.width := "320px",
              ^.onChange ==> (
                (e: ReactEventI) => {
                  val newValue = Some(EditedQuestion(keywordIndex, e.target.value))
                  scope.modState(State.validatedQuestions.modify(_.updated(index, newValue)))
                }),
              ^.onFocus --> scope.modState(State.curQuestion.set(index)),
              ^.value := newString
            )
          },
          <.div(
            ^.margin := "1px",
            ^.paddingLeft := "20px",
            ^.paddingRight := "1px", ^.paddingTop := "1px", ^.paddingBottom := "1px",
            answer
          )
        )
    }

    def render(s: State) = s match {
      case State(curQuestion, keywordPicking, validatedQuestions) =>
        WebsocketLoadable(
          WebsocketLoadableProps(
            websocketURI = websocketUri, request = SentenceRequest(prompt.path), render = {
              case Connecting => <.div("Connecting to server...")
              case Loading => <.div("Retrieving data...")
              case Loaded(SentenceResponse(sentence, alignedTokens), _) =>
                import scalaz.std.list._
                val curKeyword = s.curKeyword
                  <.div(
                    Styles.mainContent,
                    instructions,
                    <.hr(),
                    <.h2("Task"),
                    <.div(
                      <.p(
                        Styles.unselectable,
                        TextRendering.renderSentence(
                          sentence.words,
                          getToken = (word: CoNLLWord) => word.token,
                          spaceFromNextWord = (nextWord: CoNLLWord) => List(<.span(" ")),
                          renderWord = (word: CoNLLWord) => List(
                            <.span(
                              Styles.hoverBold,
                              ^.color := (
                                if(keywordPicking.fold(false)(_.contains(word.index))) "rgb(50, 164, 251)"
                                else if(curKeyword.fold(false)(_ == word.index)) "rgb(50, 164, 251)"
                                else "black"),
                              ^.onClick --> scope.modState(
                                State.keywordPicking.set(None) andThen State.validatedQuestions.modify(_.updated(
                                  curQuestion, validatedQuestions(curQuestion) match {
                                    case None => Some(
                                      EditedQuestion(
                                        word.index,
                                        TextRendering.renderSentence(prompt.sourcedTokenizedQAPairs(curQuestion).questionTokens)
                                      ))
                                    case Some(InvalidQuestion) => Some(InvalidQuestion)
                                    case Some(EditedQuestion(_, questionString)) =>
                                      Some(EditedQuestion(word.index, questionString))
                                  }))
                              ),
                              TextRendering.normalizeToken(word.token)
                            ))
                        )),
                        keywordPicking match {
                          case None => s.curKeyword match {
                            case None =>
                              <.p(Styles.niceBlue,
                                "Click your chosen key word in a question to start editing it")
                            case Some(kwIndex) =>
                              <.p(Styles.niceBlue,
                                "Click in the sentence to change the current keyword")
                          }
                          case Some(kwChoices) =>
                            <.p(Styles.badRed,
                              "Couldn't determine exact keyword; please click it in the sentence")
                        },
                      <.ul(
                        Styles.listlessList,
                        (0 until validatedQuestions.size)
                          .map(qaField(s, sentence, alignedTokens))
                          .map(field => <.li(Styles.topSep, ^.display := "block", field))
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
  def example(origQ: String, answer: String, newQ: Option[ReactElement], isGood: Boolean, tooltip: String) =
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
        You will be presented with a selection of English text and a list of at most 6 question-answer pairs about that selection
        prepared by other annotators."""),
    <.p("""You will choose a key word, correct any typos or errors, and remove unnecessary words from each question,
        while preserving its meaning. Consider the following example:"""),
    <.blockquote(<.i("The downtrodden protesters left after executive intervention reinstated the project.")),
    <.ul(
      <.li("How did the leaving protesters feel? --> downtrodden"),
      <.li("What did the protesters leave after? --> executive intervention reinstated the project")),
    <.p("""For the first question, the keyword should be """, <.b("protesters"), """ and you would revise it to """,
        <.b("How did the protesters feel? "),
        """For the second question, the keyword could be """, <.b("after"), """ and you would leave the question unchanged."""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        For each question, your revised version must satisfy the following criteria:"""),
    <.ol(
      <.li("""Spelling, punctuation, capitalization, and other errors have been fixed and the question is fluent English."""),
      <.li("""The question asks about the same thing, the answer is correct and grammatical for that question."""),
      <.li("""Any unnecessary words and phrases have been removed."""),
      <.li("""The question still contains the key word.""")),
    <.p("""You may reword the question slightly if necessary.
        For the key word, you should choose a content word in the question that you think is
        most important to the question, and relates most closely to the answer.
        The word will be taken from the sentence, but may appear in the question in a different form (e.g., "appear" versus "appearance").
        If you are unsure which word to choose, just use your best judgment about which word feels most important."""),
    <.p("""If there is no possible key word, the question is not salvageable to fluent English,
        or the answer is incorrect, you should mark it """, <.b("Invalid"), """.
        You should also mark as invalid questions that are not explicitly and obviously answered in the sentence.
        It is fine for a question to have multiple correct answers as long as the given answer is one of them.
        See the examples for further explanation."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i("""The new chairman fears a large decrease in profits in the second quarter,
            after a shaky performance in the beginning of the year.""")),
    <.p("""Here are examples of revisions you might make, with the key words bolded:"""),
    <.ul(
      Styles.listlessList,
      example(origQ = """Shaky performance resulted in an expected decrease in what?""", answer = "profits",
              newQ = Some(<.span("An expected ", <.b("decrease"), " in what?")), isGood = true,
              tooltip = """The question does not need to be a complete sentence. Keep only the words that are necessary,
                           but make sure it is fluent English."""),
      example(origQ = "What direction will profits go?", answer = "decrease",
              newQ = Some(<.span("What might ", <.b("profits "), " do?")), isGood = true,
              tooltip = """This question was almost unsalvageable, because "decrease" is not a grammatical answer.
                           It also would be acceptable to mark this question Invalid."""),
      example(origQ = "what does the chairmen fear", answer = "a decrease in profits",
              newQ = Some(<.span("What does the chairman ", <.b("fear"), "?")), isGood = true,
              tooltip = """Correct typos, capitalization, and punctuation if necessary."""),
      example(origQ = "Which quarter?", answer = "second",
              newQ = Some(<.span("Which ", <.b("quarter?"))), isGood = true,
              tooltip = """If the question is fine as-is, don't change it."""),
      example(origQ = "What went down?", answer = "profits",
              newQ = None, isGood = true,
              tooltip = """There is no viable key word in this question, so it is invalid."""),
      example(origQ = "When might profits decrease?", answer = "in the second quarter",
              newQ = Some(<.span("When might profits ", <.b("decrease"), "?")), isGood = true,
              tooltip = """The special word should be "decrease" here because the decrease
                           is what would happen "in the second quarter"."""),
      example(origQ = "What might the chairman lose?", answer = "profits",
              newQ = None, isGood = true,
              tooltip = """This asks about something that is not explicitly stated in the sentence, so it is invalid.""")
    ),
    <.p("""Your responses will be assessed by other workers.
           If your revisions are consistently rejected as low-quality,
           or if you fail to improve low-quality questions, you will be notified.
           After that, if your responses do not improve, you will be banned from this task and future tasks.
           Otherwise, your work will be approved within an hour."""),
    <.p("""There is a time limit of 3 minutes,
           but it should generally take between one and two minutes to complete each HIT."""),
    <.p("""You must provide keywords for every question, and revisions where applicable, in order to submit the HIT.
           If you have any questions, concerns, or points of confusion,
           please share them in the "Feedback" field so we may improve the task.""")
  )
}
