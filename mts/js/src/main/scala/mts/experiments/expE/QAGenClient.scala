package mts.experiments.expE

import mts.experiments._
import mts.conll._
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

object QAGenClient extends TaskClient[QAGenPrompt, QAGenResponse] {

  sealed trait HighlightingState
  case object DoingNothing extends HighlightingState
  case object Highlighting extends HighlightingState
  case object Erasing extends HighlightingState

  sealed trait State
  @Lenses case class Loading(
    message: String
  ) extends State
  @Lenses case class Loaded(
    sentence: CoNLLSentence,
    qaPairs: List[(String, Set[Int])],
    currentFocus: Int,
    highlightingState: HighlightingState
  ) extends State {
    def questionWord = sentence.words(prompt.wordIndex)
  }
  object State {
    def loading[A]: Prism[State, Loading] = GenPrism[State, Loading]
    def loaded[A]: Prism[State, Loaded] = GenPrism[State, Loaded]
  }

  val currentFocusLens = State.loaded composeLens Loaded.currentFocus
  val qaPairsLens = State.loaded composeLens Loaded.qaPairs
  val highlightingStateLens = State.loaded composeLens Loaded.highlightingState

  val emptyQA = ("", Set.empty[Int])

  class FullUIBackend(scope: BackendScope[Unit, State]) {
    def load: Callback = scope.state map {
      case Loading(_) =>
        val socket = new dom.WebSocket(websocketUri)
        socket.onopen = { (event: Event) =>
          scope.setState(Loading("Retrieving data")).runNow
          socket.send(write(SentenceRequest(prompt.path)))
        }
        socket.onerror = { (event: ErrorEvent) =>
          val msg = s"Connection failure. Error code: ${event.colno}"
          System.err.println(msg)
          // TODO maybe retry or something
        }
        socket.onmessage = { (event: MessageEvent) ⇒
          val response = read[ApiResponse](event.data.toString)
          response match {
            case SentenceResponse(path, sentence) =>
              scope.setState(Loaded(sentence, List.fill(5)(emptyQA), 0, DoingNothing)).runNow
          }
        }
        socket.onclose = { (event: Event) =>
          val msg = s"Connection lost."
          System.err.println(msg)
          // TODO maybe retry or something
        }
      case Loaded(_, _, _, _) =>
        System.err.println("Data already loaded.")
    }

    def updateResponse: Callback = scope.state.map {
      st => qaPairsLens.getOption(st).map(QAGenResponse.apply).foreach(setResponse)
    }

    def qaField(loadedState: Loaded, index: Int, bonus: Option[Int]) = loadedState match {
      case ls @ Loaded(sentence, qaPairs, currentFocus, _) =>
        val isFocused = loadedState.currentFocus == index
        val isAnswerEmpty = qaPairs(index)._2.isEmpty
        <.div(
          ^.overflow := "hidden",
          <.div(
            ^.float := "left",
            ^.width := "25px",
            ^.minHeight := "1px",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.textAlign := "right",
            (bonus.map(b => s"+${b}c").getOrElse(""): String)
          ),
          <.input(
            ^.float := "left",
            ^.`type` := "text",
            ^.required := index == 0,
            ^.placeholder := s"""Question about "${TextRendering.normalizeToken(ls.questionWord.token)}"""",
            ^.margin := "1px",
            ^.marginLeft := "26px",
            ^.padding := "1px",
            ^.width := "240px",
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  qaPairsLens.modify(
                    qaPairs => {
                      val currentQA = qaPairs(index)
                      val newQA = currentQA.copy(_1 = newValue)
                      qaPairs.updated(index, newQA)
                    }))
              }),
            ^.onFocus --> scope.modState(currentFocusLens.set(index)),
            ^.value := qaPairs(index)._1
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
              "Highlight your answer above"
            } else {
              TextRendering.renderSentence(sentence.words.filter(w => qaPairs(index)._2.contains(w.index)).map(_.token))
            }
          )
        )
    }

    def touchWord(index: Int): Callback = scope.modState {
      case s @ Loading(_) => s
      case s @ Loaded(sentence, qaPairs, currentFocus, highlightingState) =>
        val answerSpans = qaPairs.map(_._2)
        val curAnswer = answerSpans(currentFocus)
        highlightingState match {
          case DoingNothing => s
          case Highlighting =>
            if(!curAnswer.contains(index) && !answerSpans.flatten.contains(index)) {
              qaPairsLens.modify(
                qaPairs => {
                  val currentQA = qaPairs(currentFocus)
                  val newQA = currentQA.copy(_2 = currentQA._2 + index)
                  qaPairs.updated(currentFocus, newQA)
                })(s)
            } else s
          case Erasing =>
            if(curAnswer.contains(index)) {
              qaPairsLens.modify(
                qaPairs => {
                  val currentQA = qaPairs(currentFocus)
                  val newQA = currentQA.copy(_2 = currentQA._2 - index)
                  qaPairs.updated(currentFocus, newQA)
                })(s)
            } else s
        }
    }

    def render(s: State) = {

      <.div(
        ^.onMouseUp --> scope.modState(highlightingStateLens.set(DoingNothing)),
        ^.onMouseDown --> scope.modState(highlightingStateLens.set(Highlighting)),
        Styles.mainContent,
        instructions,
        <.hr(),
        s match {
          case Loading(msg) =>
            <.p(s"Loading sentence ($msg)...")
          case ls @ Loaded(sentence, qaPairs, currentFocus, highlightingState) =>
            val answerSpans = qaPairs.map(_._2)
            val curAnswer = answerSpans(currentFocus)
            import scalaz.std.list._
            <.div(
              <.p(
                Styles.unselectable,
                TextRendering.renderSentence(
                  sentence.words,
                  (word: CoNLLWord) => word.token,
                  (nextWord: CoNLLWord) => List(
                    <.span(
                      ^.backgroundColor := (
                        if(curAnswer.contains(nextWord.index) && curAnswer.contains(nextWord.index - 1)) {
                          "#FFFF00"
                        } else if(!curAnswer.contains(nextWord.index) &&
                                    !curAnswer.contains(nextWord.index - 1) &&
                                    answerSpans.flatten.contains(nextWord.index) &&
                                    answerSpans.flatten.contains(nextWord.index - 1)) {
                          "#CCCCCC"
                        } else {
                          "transparent"
                        }),
                      " ")),
                  (word: CoNLLWord) => List(
                    <.span(
                      word.index == prompt.wordIndex ?= Styles.questionWord,
                      ^.backgroundColor := (
                        if(curAnswer.contains(word.index)) {
                          "#FFFF00"
                        } else if(answerSpans.flatten.contains(word.index)) {
                          "#CCCCCC"
                        } else {
                          "transparent"
                        }
                      ),
                      ^.onMouseMove --> touchWord(word.index),
                      ^.onMouseDown ==> (
                        (e: ReactEventI) => if(curAnswer.contains(word.index)) {
                          e.stopPropagation
                          scope.modState(highlightingStateLens.set(Erasing)) >> touchWord(word.index)
                        } else {
                          scope.modState(highlightingStateLens.set(Highlighting)) >> touchWord(word.index)
                        }
                      ),
                      TextRendering.normalizeToken(word.token)
                    ))
                )),
              <.ul(
                Styles.listlessList,
                (0 until qaPairs.size).map(i =>
                  <.li(
                    ^.display := "block",
                    qaField(ls, i, Some(3 * i).filter(_ != 0))
                  )
                )
              )
            )
        }
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(Loading("Connecting to server"): State)
    .renderBackend[FullUIBackend]
    .componentDidMount(context => context.backend.load)
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  // private[this] val introBlurb = <.p(<.b(
  //   """Write questions and their answers about the chosen word in the following selection of English text.
  //      Please read the instructions below in detail before beginning."""))

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p(<.span("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of English sentences into a list of questions and answers.
        For each HIT, you will be presented with a selection of English text,
        and a specific (bolded and underlined) content word from that selection, called the """), <.b("question word.")),
    <.p("""Briefly, your task is to write at least one question containing the question word whose answer is a phrase in the sentence.
        You may earn extra by writing more questions and answers. For each HIT,
        you will be rewarded bonuses based on how many of your question-answer pairs are valid.
        Please read all of the instructions below for details on what constitutes a valid response."""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Your response must satisfy the following criteria:"""),
    <.ol(
      <.li("""The question contains the question word, and as few other words from the sentence as possible."""),
      <.li("""The answer is the longest phrase from the sentence that answers the question without extra unnecessary information."""),
      <.li("""The answers to your questions may not overlap in the sentence.""")
    ),
    <.p("""Answers don't have to be contiguous phrases; you can skip a word or two if it provides a more natural answer,
        though this usually will not be the case. See the examples below for details."""),
    <.h2("""Examples"""),
    <.p("""Consider the following sentence:"""),
    <.blockquote(<.i("""I take full and complete responsibility for my decision to disclose these materials to the public. """)),
    <.p("""Depending on the question word you receive, acceptable questions and answers include (but are not limited to):"""),
    <.ul(
      <.li(<.span("Who "), <.b("takes "), <.span("something? --> I")),
      <.li(<.span("What does someone "), <.b("take"), <.span("? --> full and complete responsibility for my decision to disclose these materials to the public")),
      <.li(<.span("What is "), <.b("full"), <.span("? --> responsibility")),
      <.li(<.span("What level of "), <.b("responsibility"), <.span("? --> full and complete")),
      <.li(<.span("Who is "), <.b("responsible "), <.span("for something? --> I")),
      <.li(<.span("Whose "), <.b("decision "), <.span("was it? --> my decision")),
      <.li(<.span("Who "), <.b("decided "), <.span("to do something? --> I")),
      <.li(<.span("What did someone "), <.b("decide "), <.span("to do? --> disclose these materials to the public")),
      <.li(<.span("Which "), <.b("materials"), <.span("? --> these materials"))
    ),
    <.p("""Note because of pronouns like "I" and "you", you have to write your questions and answers
        from the perspective of the speaker of the sentence.
        You may also change the form of the word you're using in the question,
        and sometimes (though usually not) the answer will contain the question word as well.
        It is often useful to change nouns to verbs, as with the word "decision" where you may ask who "decided" something
        and what they "decided" to do."""),
    <.p("""However, it must be obvious from your question which word is the question word.
        If you abuse this and write long questions, questions that use many words from the sentence,
        or questions that ignore the question word entirely, you will be banned.
        It is useful to use "someone" and "something," as well as phrases like "do something,"  wherever possible
        to avoid confusion over the question word."""),
    <.p("""Now consider the following sentence:"""),
    <.blockquote(<.i("""Book and movie pirates who have downloaded 100 files or less tend not to consider themselves criminals.""")),
    <.p("""Acceptable questions and answers include:"""),
    <.ul(
      <.li(<.span("What kind of "), <.b("pirates"), <.span("? --> Book and movie pirates")),
      <.li(<.span("What does someone "), <.b("pirate"), <.span("? --> Book and movie")),
      <.li(<.span("How many "), <.b("files"), <.span("? --> 100 or less"))
    ),
    <.p("""Questions beginning with "What kind" tend to be useful for adjectives and other descriptors.
        In some cases, the words from the sentence may not quite be grammatical: for example, "books and movies"
        would be a better answer to the second question above, but since we restrict answers to be direct words from the sentence,
        we tolerate these slightly-off answers as long as they are the best available and the meaning is obvious.
        You may also notice in the last answer above that the word "files" is skipped to provide for a more natural answer
        that only expresses the number."""),
    <.h2("""Conditions"""),
    <.p("""If your work satisfies the above criteria, it will be approved in at most one hour.
          If your responses repeatedly fail to meet the above criteria, you will be blocked from this task and future tasks.
          Each HIT should take less than one minute to complete, depending on how many questions and answers you choose to write."""),
    <.h2("""Bonuses"""),
    <.p("""Depending on how many of your question-answer pairs are accepted as valid, you will receive a bonus.
        The first question-answer pair is required, for which you will receive the flat payment for the HIT.
        For each additional valid question-answer pair, you will receive a progressively increasing bonus,
        listed next to each question field below.
        For example, since the second one gives a bonus of 3c and the third gives a bonus of 6c,
        if you submit 3 valid question-answer pairs you will get a bonus of 9c.
        To get the most out of this task, try to find a good time balance between reading new sentences
        and spending time thinking of more specific questions that won't be rejected.
        In many cases there will be only one reasonable question, and this is fine:
        you will be better off submitting it for the 5c reward and moving on to the next HIT.
        But in other cases, it could be worth your while to try to cover as much of the sentence as possible with your answers."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field so we may improve the task.""")
    )
}
