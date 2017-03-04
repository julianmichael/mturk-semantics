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

object GenerationClient extends TaskClient[SentenceId, List[WordedQAPair]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
    jQuery(s"#$submitButtonLabel").hide()
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[GenerationApiRequest, GenerationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int)] // qa index, answer word index
  import HighlightingComponent._

  @Lenses case class State(
    pastQAGroups: List[List[WordedQAPair]], // each group == 1 keyword
    curKeyword: Option[Int],
    curQAs: List[(String, Set[Int])],
    curFocus: Int) {
    def wordStats(sentence: Vector[String], admissible: Int => Boolean): WordStats = sentence
      .indices
      .filter(admissible).map { i =>
      val qasForIndex = pastQAGroups.find(g => g.head.wordIndex == i)
      i -> WordStat(
        index = i,
        numPrompted = qasForIndex.size,
        numQAPairs = qasForIndex.fold(0)(_.size),
        numQAPairsContaining = pastQAGroups.flatten.filter(wqa =>
          wqa.wordIndex == i ||
            wqa.question.toLowerCase.split("\\s+").contains(sentence(i).toLowerCase) ||
            wqa.answer.contains(i))
          .size
      )
    }.toMap
  }
  object State {
    val initial = State(Nil, None, newQAs, 0)
  }

  val emptyQA = ("", Set.empty[Int])
  val newQAs = List.fill(numQAs)(emptyQA)

  def isComplete(pair: (String, Set[Int])) = !pair._1.isEmpty && !pair._2.isEmpty

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.curQAs.modify(curQAs =>
          curQAs.zipWithIndex.map {
            case ((question, _), qaIndex) => (
              question, hs.span.collect {
                case (hlQAIndex, aIndex) if hlQAIndex == qaIndex => aIndex
              }.toSet)
          }
        )
      )

    def updateResponse: Callback = scope.state.map { st =>
      val curQAs = st.curKeyword.map(keyword =>
        st.curQAs.filter(isComplete).map(p =>
          WordedQAPair(keyword, p._1, p._2)
        )
      )
      val allQAs = curQAs.fold(st.pastQAGroups)(_ :: st.pastQAGroups).flatten
      setResponse(allQAs)
    }

    def qaField(s: State, sentence: Vector[String], index: Int, bonus: Double, curNumQAPairs: Int) = s match {
      case s @ State(_, curKeyword, curQAs, curFocus) =>
        val isFocused = curFocus == index
        val charsLeft = questionCharLimit - curQAs(index)._1.length
        val isAnswerEmpty = curQAs(index)._2.isEmpty
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
          <.div(
            curNumQAPairs > index ?= Styles.goodGreen,
            curNumQAPairs > index ?= Styles.bolded,
            ^.float := "left",
            ^.width := "15px",
            ^.minHeight := "1px",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.textAlign := "right",
            (bonus != 0.0) ?= s"+${math.round(100 * bonus).toInt}c"
          ),
          <.input(
            (curNumQAPairs < index || isNotAssigned) ?= (^.disabled := true),
            (curNumQAPairs < index || isNotAssigned) ?= (^.backgroundColor := "#EEEEEE"),
            ^.float := "left",
            ^.`type` := "text",
            ^.placeholder := s"Question",
            ^.margin := "1px",
            ^.marginLeft := "26px",
            ^.padding := "1px",
            ^.width := "240px",
            ^.maxLength := questionCharLimit,
            ^.onChange ==> (
              (e: ReactEventI) => {
                val newValue = e.target.value
                scope.modState(
                  State.curQAs.modify(
                    qas => {
                      val currentQA = qas(index)
                      val newQA = currentQA.copy(_1 = newValue)
                      qas.updated(index, newQA)
                    }))
              }),
            ^.onFocus --> scope.modState(State.curFocus.set(index)),
            ^.value := curQAs(index)._1
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
                sentence.zipWithIndex.filter(p => curQAs(index)._2.contains(p._2)).map(_._1)
              )
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = GenerationApiRequest(prompt),
          onLoad = r => scope.modState(State.curKeyword.set(Some(WordStats.indicesByPriority(r.wordStats).head))),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(GenerationApiResponse(sentence, wordStats), _) =>
              import scalaz.std.list._
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateHighlights, render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(setSpan, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val maxNumKeywords = math.min(numKeywords, WordStats.indicesByPriority(wordStats).size)
                      val curKeywordNum = s.pastQAGroups.size + 1

                      val curNumQAPairs = s.curQAs.filter(isComplete).size

                      val curPotentialBonus = (s.curQAs.filter(isComplete) :: s.pastQAGroups)
                        .flatMap(g => g.zip(bonuses).map(_._2)).sum

                      val curAnswer = spans.collect {
                        case (qaIndex, ansIndex)
                            if qaIndex == s.curFocus => ansIndex
                      }
                      def touchWord(i: Int) = s.curKeyword.fold(Callback.empty)(curKeyword =>
                        touchElement((s.curFocus, i))
                      )
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
                                s.curKeyword.fold(false)(_ == index) ?= Styles.specialWord,
                                s.curKeyword.fold(false)(_ == index) ?= Styles.niceBlue,
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
                        <.ul(
                          Styles.listlessList,
                          (0 until s.curQAs.size).map(i =>
                            <.li(
                              ^.display := "block",
                              qaField(s, sentence, i, bonuses(i), curNumQAPairs)
                            )
                          )
                        ),
                        <.input(
                          ^.`type` := (if(curKeywordNum >= maxNumKeywords) "submit" else "button"),
                          ^.disabled := curNumQAPairs < 1,
                          ^.margin := "1px",
                          ^.padding := "5px",
                          ^.color := "white",
                          ^.backgroundColor := (
                            if(curNumQAPairs < 1) "#CCCCCC"
                            else if(curKeywordNum >= maxNumKeywords) "rgb(48, 140, 20)"
                            else "rgb(50, 164, 251)"
                          ),
                          ^.onClick --> (
                            if(curNumQAPairs < 1 || curKeywordNum >= maxNumKeywords) Callback.empty else {
                              s.curKeyword.fold(Callback.empty) { curKeyword =>
                                val qasFinished = s.curQAs
                                  .filter(isComplete)
                                  .map(p => WordedQAPair(curKeyword, p._1, p._2))
                                val finishedKeywords = s.pastQAGroups.flatten.map(_.wordIndex).toSet + curKeyword
                                val newKeyword = WordStats
                                  .indicesByPriority(WordStats.merge(wordStats, s.wordStats(sentence, wordStats.keySet)))
                                  .filterNot(finishedKeywords)
                                  .head
                                scope.setState(State(qasFinished :: s.pastQAGroups, Some(newKeyword), newQAs, 0)) >>
                                  setSpan(Set.empty[(Int, Int)])
                              }
                            }
                          ),
                          ^.value := (
                            if(curKeywordNum >= maxNumKeywords) {
                              "Submit"
                            } else {
                              s"Next word (${curKeywordNum - 1}/$maxNumKeywords complete)"
                            }
                          )
                        ),
                        <.span(
                          ^.paddingLeft := "5px",
                          "Potential bonus: ",
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
        You will be presented with a selection of English text with a """), <.b("special word"), " written in bold blue."),
    <.p("""You will write questions and their answers, where the answer is taken from the sentence and """,
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
      <.li("""You do not repeat the same information between multiple question-answer pairs,
              even for different special words."""),
      <.li("""The question is open-ended: no yes/no or either/or questions.""")
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
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i("""Alex """, <.span(Styles.specialWord, "pushed"), """ Chandler at school today.""")),
    <.p("""Consider the following question-answer pairs:"""),
    <.ul(
      <.li(<.div("What did Alex do? --> ", <.span(Styles.specialWord, "pushed"), " Chandler at school today")),
      <.li(<.div("Who ", <.span(Styles.specialWord, "pushed"), " Chandler? --> Alex"))
    ),
    <.p("These are each acceptable on their own, but ", <.b("you may not provide both, "),
        """because they convey the same information, just reversing the order of the question and answer.
        One way to notice this is that the answer to one question ("Alex") appears in the other question."""),
    <.ul(
      <.li(<.div("When did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today")),
      <.li(<.div("On what day did someone  ", <.span(Styles.specialWord, "push"), " someone? --> today"))
    ),
    <.p("""Here, the second is just a minor rephrasing of the first, which is not acceptable.
        One way to notice this is that both questions have the same answer."""),
    <.p("Mouse over the following examples of ", <.b("bad"), " question-answer pairs for explanations:"),
    <.ul(
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Where did Alex do something? --> at school"),
                 <.span(^.className := "tooltiptext",
                        """This fails to include the special word "push" in either the question or answer."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Did Alex or Chandler push someone? --> Alex"),
                 <.span(^.className := "tooltiptext",
                        """Either/or and yes/no questions are not allowed."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Where did Alex "), <.b("push "), <.span("Chandler? --> at school today"),
                 <.span(^.className := "tooltiptext",
                        """The question asked "where", so including the word "today" is incorrect.""")))
    ),
    <.h2("""Conditions & Bonuses"""),
    <.p("""For each HIT, you will be shown up to four special words from the sentence.
          You are required to write at least one question-answer pair for each special word.
          However, you will receive progressively increasing bonuses (listed next to each question entry field)
          if you can come up with more.
          How long the HIT takes will depend on how many question-answer pairs you write;
          you should aim to take less than 30 seconds per question-answer pair (so around two minutes per HIT).
          """),
    <.p(""" Your work will be evaluated by other workers to see whether it meets the requirements outlined here. """,
          <.b("""You will only be awarded bonuses according to the number of GOOD question-answer pairs you write, """),
          """as judged by other workers.
          So the bonus you should expect to receive depends on the quality of your responses.
          Since other workers will occasionally make mistakes, this means you might not always get the full bonus amount
          indicated as the "potential bonus," even if your responses are valid.
          Your bonus will be awarded as soon as validators have checked all of your question-answer pairs.
          If your responses are too frequently rejected by validators,
          you will first be warned, then blocked from this task and future tasks."""),
    <.h2("""Tips"""),
    <.ul(
      <.li(s"""To make the task go quickly, make your questions as short as possible.
             (There is a ${questionCharLimit}-character limit.)"""),
      <.li("It is better to move on to the next word than add an awkward extra question."),
      <.li("""As you work on the task, you will discover some broadly-applicable templates for questions.
              Keep these in mind and it can potentially speed you up."""),
      <.li("""Feel free to use generic words like "someone" and "something" to simplify your questions
              and make them shorter.""")
    ),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field.""")
    )
}
