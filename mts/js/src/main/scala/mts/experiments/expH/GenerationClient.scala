package mts.experiments.expH

import mts.experiments._
import mts.conll._
import mts.tasks._
import mts.language._
import mts.util.dollarsToCents
import mts.experiments.expF.WebsocketLoadableComponent

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

object GenerationClient extends TaskClient[GenerationPrompt, List[WordedQAPair]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[GenerationApiRequest, GenerationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int)] // keyword INDEX among keywords, qa index, answer word index
  import HighlightingComponent._

  @Lenses case class State(
    qaGroups: List[List[WordedQAPair]], // each group == 1 keyword
    curFocus: (Int, Int)) {
  }
  object State {
    val initial = State(prompt.keywords.map(kw => List(emptyQA(kw))), (0, 0))
  }

  def emptyQA(keyword: Int) = WordedQAPair(keyword, "", Set.empty[Int])

  def isComplete(wqa: WordedQAPair) = !wqa.question.isEmpty && !wqa.answer.isEmpty

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def addQAFields: (State => State) =
      State.qaGroups.modify(groups =>
        groups.map(group =>
          if(group.filter(isComplete).size == group.size) group ++ List(emptyQA(group.head.wordIndex))
          else group
        )
      )

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.modify(groups =>
          groups.zipWithIndex.map {
            case (group, gIndex) =>
              group.zipWithIndex.map {
                case (WordedQAPair(keyword, question, _), qaIndex) => WordedQAPair(
                  keyword, question, hs.span.collect {
                    case (`gIndex`, `qaIndex`, aIndex) => aIndex
                  }.toSet)
              }
          }
        ) andThen addQAFields
      )

    def updateResponse: Callback = scope.state.map { st =>
      setResponse(st.qaGroups.flatten.filter(isComplete))
      setSubmitEnabled(st.qaGroups.forall(_.filter(isComplete).size > 0))
    }

    def qaField(s: State, sentence: Vector[String], groupIndex: Int, qaIndex: Int) = s match {
      case State(qaGroups, curFocus) =>
        val isFocused = curFocus == (groupIndex, qaIndex)
        val numQAsInGroup = qaGroups(groupIndex).size
        val WordedQAPair(_, question, answer) = s.qaGroups(groupIndex)(qaIndex)
        val charsLeft = questionCharLimit - question.length
        val isAnswerEmpty = answer.isEmpty
        val nextBonus = bonusFor(qaGroups.map(_.drop(1)).flatten.filter(isComplete).size + 1)
        <.div(
          ^.overflow := "hidden",
          <.div(
            Styles.badRed,
            ^.float := "left",
            ^.width := "10px",
            ^.minHeight := "1px",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.textAlign := "right",
            (charsLeft <= 10 && isFocused) ?= s"$charsLeft"
          ),
          <.input(
            // ((index - 1) == numQAsInGroup || isNotAssigned) ?= (^.disabled := true),
            // ((index - 1) == numQAsInGroup || isNotAssigned) ?= (^.backgroundColor := "#EEEEEE"),
            ^.float := "left",
            ^.`type` := "text",
            ^.placeholder := (
              if(qaIndex == 0) "Question (required)"
              else s"Question (+${math.round(100 * nextBonus).toInt}c)"
            ),
            ^.margin := "1px",
            ^.padding := "1px",
            ^.width := "240px",
            ^.maxLength := questionCharLimit,
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  State.qaGroups.modify(groups =>
                    groups.updated(
                      groupIndex,
                      groups(groupIndex).updated(
                        qaIndex,
                        groups(groupIndex)(qaIndex).copy(question = newValue))
                    )) andThen addQAFields)
              }),
            ^.onFocus --> scope.modState(State.curFocus.set((groupIndex, qaIndex))),
            ^.value := question
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
              TextRendering.renderSentence(
                sentence.zipWithIndex.filter(p => answer.contains(p._2)).map(_._1)
              )
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = GenerationApiRequest(prompt.path),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(GenerationApiResponse(sentence), _) =>
              import scalaz.std.list._
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateHighlights, render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(setSpan, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val curCompleteQAPairs = s.qaGroups.flatten.filter(isComplete)

                      val curPotentialBonus = (1 to (curCompleteQAPairs.size - s.qaGroups.size))
                        .map(bonusFor).sum

                      val (curGroupIndex, curQAIndex) = s.curFocus

                      val curAnswer = spans.collect {
                        case (`curGroupIndex`, `curQAIndex`, ansIndex) => ansIndex
                      }
                      def touchWord(i: Int) = touchElement((curGroupIndex, curQAIndex, i))
                      <.div(
                        ^.onMouseUp --> stopHighlight,
                        ^.onMouseDown --> startHighlight,
                        Styles.mainContent,
                        instructions,
                        <.hr(),
                        <.p(
                          Styles.unselectable,
                          TextRendering.renderSentence(
                            sentence.indices,
                            (index: Int) => sentence(index),
                            (nextIndex: Int) => List(
                              <.span(
                                ^.backgroundColor := (
                                  if(curAnswer.contains(nextIndex) && curAnswer.contains(nextIndex - 1)) {
                                    "#FFFF00"
                                  } else {
                                    "transparent"
                                  }),
                                " ")),
                            (index: Int) => List(
                              <.span(
                                prompt.keywords.contains(index) ?= Styles.specialWord,
                                prompt.keywords.contains(index) ?= Styles.niceBlue,
                                ^.backgroundColor := (
                                  if(curAnswer.contains(index)) {
                                    "#FFFF00"
                                  } else {
                                    "transparent"
                                  }
                                ),
                                ^.onMouseMove --> touchWord(index),
                                ^.onMouseDown ==> (
                                  (e: ReactEventI) => if(curAnswer.contains(index)) {
                                    e.stopPropagation
                                    startErase >> touchWord(index)
                                  } else {
                                    startHighlight >> touchWord(index)
                                  }
                                ),
                                TextRendering.normalizeToken(sentence(index))
                              ))
                          )),
                        <.div(
                          (0 until s.qaGroups.size).map(groupIndex =>
                            <.div(
                              <.p(
                                Styles.bolded,
                                TextRendering.normalizeToken(sentence(prompt.keywords(groupIndex)))
                              ),
                              <.ul(
                                Styles.listlessList,
                                (0 until s.qaGroups(groupIndex).size).map(qaIndex =>
                                  <.li(
                                    ^.display := "block",
                                    qaField(s, sentence, groupIndex, qaIndex)
                                  )
                                )
                              )
                            )
                          )
                        ),
                        <.p(
                          "Potential bonus so far: ",
                          <.span(
                            curPotentialBonus > 0 ?= Styles.goodGreen,
                            curPotentialBonus > 0 ?= Styles.bolded,
                            s"${math.round(100 * curPotentialBonus).toInt}c"
                          )
                        )
                      )
                  }))
          }))
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(context => context.$.backend.updateResponse)
    .build

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p(<.span("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of English sentences into a list of questions and answers.
        You will be presented with a selection of English text with a set of """), <.b("special words"), " written in bold."),
    <.p("""For each special word, you will write questions and their answers, where the answer is taken from the sentence and """,
        <.b("""either the question or the answer contains the special word. """),
        """You will earn bonuses by writing more questions and answers.
        For example, consider the sentence:"""),
    <.blockquote(<.i("The jubilant ", <.span(Styles.specialWord, "protesters"),
                     " celebrated after executive intervention canceled the project.")),
    <.p("""Valid question-answer pairs include:"""),
    <.ul(
      <.li("How did the ", <.b("protesters "), "feel? --> jubilant"),
      <.li("Who celebrated? --> the ", <.b("protesters"))),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Your response must be grammatical, fluent English that satisfies the following criteria:"""),
    <.ol(
      <.li("""Either the question or the answer contains the special word."""),
      <.li("""The question contains at least one word from the sentence."""),
      <.li("""The answer is the longest natural, correct, and unique answer to the question."""),
      <.li("""None of your question-answer pairs are redundant with each other,
              even for different special words."""),
      <.li("""The question is open-ended: yes/no and either/or questions are not allowed.""")
    ),
    <.p("See the examples for further explanation."),
    <.h2("""Good Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i("""I take full and complete responsibility for my thoughtless """, <.span(Styles.specialWord, """decision"""),
                     """ to disclose these materials to the public. """)),
    <.p("""Acceptable questions and answers include, but are not limited to, the following.
        Mouse over each example to see an explanation."""),
    <.ul(
      <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
                 <.span("Who "), <.b("decided "), <.span("something? --> I"),
                 <.span(^.className := "tooltiptext",
                        """Where possible, change nouns like "decision" to verbs in order to write short questions about them."""))),
      <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
                 <.span("What am I responsible for? --> my thoughtless "), <.b("decision"),
                 <.span(" to disclose these materials to the public"),
                 <.span(^.className := "tooltiptext",
                        "Prefer the longest answer that correctly and naturally answers the question."))),
      <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
                 <.span("What kind of "), <.b("decision"), <.span("? --> thoughtless"),
                 <.span(^.className := "tooltiptext",
                        """To get descriptive words as answers, you may need to ask "What kind" or similar questions.""")))
    ),
    <.h2("""Bad Examples"""),
    <.p("""Suppose you are given the following sentence:"""),
    <.blockquote(<.i("""Alex """, <.span(Styles.specialWord, "pushed"), """ Chandler at school today.""")),
    <.p("Mouse over the following examples of ", <.b("bad"), " question-answer pairs for explanations:"),
    <.ul(
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Who got hurt? --> Chandler"),
                 <.span(^.className := "tooltiptext",
                        """The question must include some content word from the sentence, which this fails to do."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Did Alex or Chandler push someone? --> Alex"),
                 <.span(^.className := "tooltiptext",
                        """Either/or and yes/no questions are not allowed."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Where did Alex push Chandler? --> at school today"),
                 <.span(^.className := "tooltiptext",
                        """The question asked "where", so including the word "today" is incorrect.""")))
    ),
    <.h2("Redundancy"),
    <.p("""None of your question-answer pairs in one HIT should be redundant with each other.
        To clarify what this means, consider the following question-answer pairs about the sentence above:"""),
    <.ul(
      <.li(<.div("When did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today")),
      <.li(<.div("On what day did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today"))
    ),
    <.p("""The second is just a minor rephrasing of the first, which is not acceptable.
        One way to notice this is that both questions have the same answer.
        However, suppose the special word was """, <.b("school"), """. Consider the following three question-answer pairs:"""),
    <.ol(
      <.li(<.div("Where did someone push someone? --> at ", <.b("school"))),
      <.li(<.div("Where did someone push someone today? --> at ", <.b("school"))),
      <.li(<.div("Where was Alex today? --> at ", <.b("school")))
    ),
    <.p("""Numbers 1 and 2 are redundant with each other,
        because they are asking the same question but with different amounts of detail.
        Number 3, however, is not redundant, because it is asking about the location of Alex and not the pushing.
        Finally, consider the following:"""),
    <.ul(
      <.li(<.div("What did Alex do? --> ", <.span(Styles.specialWord, "pushed"), " Chandler at school today")),
      <.li(<.div("Who ", <.span(Styles.specialWord, "pushed"), " Chandler? --> Alex"))
    ),
    <.p("""These are redundant with each other because they convey the same information,
        but just reverse the order of the question and answer.
        One way to notice this is that the answer to one question ("Alex") appears in the other question."""),
    <.h2("""Conditions & Bonuses"""),
    <.p("""For each HIT, you will be shown up to four special words from the sentence.
          You are required to write at least one question-answer pair for each special word.
          However, you will receive bonuses if you come up with more.
          (As you write question-answer pairs, new fields will appear for you to write more.)
          The bonus per question increases by 1 cent for each one you write;
          your goal is to, if possible, present the complete non-redundant set of questions and answers
          that relate the special words to each other and the rest of the sentence.
          On average, it should take less than 30 seconds per question-answer pair.
          """),
    <.p("""Your work will be evaluated by other workers according to the above criteria. """,
          <.b("""You will only be awarded bonuses for your good, non-redundant question-answer pairs, """),
          """as judged by other workers.
          This means the "total potential bonus" indicator is just an upper bound on what you may receive,
          which will depend on the quality of your responses.
          Since other workers will occasionally make mistakes, you might not always get the full
          amount even if your responses are valid.
          Your bonus will be awarded as soon as validators have checked all of your question-answer pairs,
          which will happen shortly after you submit (but will vary depending on worker availability).
          If your responses are too frequently rejected by validators,
          you will be sent a warning; if your responses do not improve,
          then you will be blocked from this task and future tasks."""),
    <.h2("""Tips"""),
    <.ul(
      <.li(s"""To make the task go quickly, make your questions as short as possible.
             (There is a ${questionCharLimit}-character limit.)"""),
      <.li("""As you work on the task, you will discover some broadly-applicable templates for questions.
              Keep these in mind and it can potentially speed you up."""),
      <.li("""Feel free to use generic words like "someone" and "something" to simplify your questions
              and make them shorter. This can also help you avoid accidental redundancies."""),
      <.li("""Start by writing the required questions so you don't end up having to move your responses around.""")
    ),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field.""")
    )
}
