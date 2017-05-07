package mts.experiments.expH

import mts.experiments._
import mts.tasks._
import mts.util.dollarsToCents

import nlpdata.datasets.conll._

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
            isNotAssigned ?= (^.disabled := true),
            ^.float := "left",
            ^.`type` := "text",
            ^.placeholder := (
              if(qaIndex == 0) "Question (required)"
              else s"Question (+${math.round(100 * nextBonus).toInt}c)"
            ),
            ^.margin := "1px",
            ^.padding := "1px",
            // ^.width := "240px",
            ^.size := questionCharLimit,
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
              Text.renderSpan(sentence, answer)
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = GenerationApiRequest(prompt.id),
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
                        <.p(<.span(Styles.badRed, """ Please read the detailed instructions at the bottom before you begin, """),
                            """ so you can maximize your bonuses and avoid losing your qualification. """,
                            """ To begin working on this HIT, please request the question-answer writing accuracy qualification.
                                It is auto-granted. Also, while there may be few HITs available at any one time,
                                more will be continuously uploaded as they are completed. """),
                        <.hr(),
                        <.p(
                          Styles.unselectable,
                          Text.render(
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
                                Text.normalizeToken(sentence(index))
                              ))
                          )),
                        <.div(
                          (0 until s.qaGroups.size).map(groupIndex =>
                            <.div(
                              <.p(
                                Styles.bolded,
                                Text.normalizeToken(sentence(prompt.keywords(groupIndex)))
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
                        ),
                        <.p(
                          <.input(
                            ^.`type` := "text",
                            ^.name := feedbackLabel,
                            ^.placeholder := "Feedback? (Optional)",
                            ^.margin := "1px",
                            ^.padding := "1px",
                            ^.width := "484px"
                          )
                        ),
                        <.input(
                          ^.`type` := "submit",
                          ^.disabled := !s.qaGroups.forall(_.filter(isComplete).size > 0),
                          ^.id := submitButtonLabel,
                          ^.value := "submit"),
                        instructions
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

  def example(question: String, answer: String, isGood: Boolean, tooltip: String) =
    <.li(
      <.div(
        isGood ?= Styles.goodGreen,
        !isGood ?= Styles.badRed,
        ^.className := "tooltip",
        <.span(question),
        <.span(" --> "),
        <.span(answer),
        <.span(^.className := "tooltiptext", tooltip)
      )
    )

  private[this] val instructions = <.div(
    <.h2("""Task Summary"""),
    <.p(<.span("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of English sentences into lists of questions and answers.
        You will be presented with a selection of English text with a set of """), <.b("special words"), " written in bold."),
    <.p("""For each special word, you will write questions and their answers, where the answer is taken from the sentence and
           either """, <.b("""the question or the answer """),
        """contains the special word. """,
        <.b("""You will earn bonuses by writing more questions and answers. """),
        """For example, consider the sentence:"""),
    <.blockquote(<.i("The jubilant ", <.span(Styles.specialWord, "protesters"),
                     " celebrated after executive intervention canceled the project.")),
    <.p("""Valid question-answer pairs include:"""),
    <.ul(
      <.li("How did the ", <.b("protesters "), "feel? --> jubilant"),
      <.li("Who celebrated? --> the ", <.b("protesters"))),
    <.p(<.b("""Warning: """), """The text shown to you is drawn randomly
           from Wikipedia and news articles from the past few years.
           We have no control over the contents of the text, which may discuss sensitive subjects,
           including crime and death, or occasionally contain offensive ideas. Please use appropriate discretion.
           (If you receive a selection of text that is not in English, please skip or return the HIT and let us know.)"""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        Your response must be grammatical, fluent English that satisfies the following criteria:"""),
    <.ol(
      <.li("""Either the question or the answer contains the special word."""),
      <.li("""The question contains at least one word from the sentence."""),
      <.li("The question is about the meaning of the sentence (and not, for example, the order of the words)."),
      <.li("""The question is answered obviously and explicitly in the sentence."""),
      <.li("""The question is open-ended: yes/no and either/or questions are not allowed."""),
      <.li("""None of your question-answer pairs are redundant with each other,
              even for different special words.""")
    ),
    <.p("""See the examples for further explanation."""),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i(""" In the year since the regulations were enacted,
                         the Director of the Environmental Protection Agency (EPA),
                         Gina McCarthy, has been aggressive in enforcing them.""")),
    <.p("""Here are questions and answers that someone may write
        (ignoring the special word requirement for now).
        Good ones are green while examples of bad questions are in red.
        Mouse over each example to see an explanation."""),
    <.ul(
      example(question = "What was enacted?", answer = "the regulations", isGood = true,
              tooltip = """This is a standard, straightforward question that is answered literally by the sentence.
                           Most questions should look something like this."""),
      example(question = "In the what since?", answer = "year", isGood = false,
              tooltip = """This simply replaces a word with "what"
                           instead of using it to form a proper English question."""),
      example(question = "How long was it since the regulations were enacted?", answer = "the year", isGood = true,
              tooltip = """While "a year" is a more natural answer, "the year" is the closest you can get
                           and the question is answered in the sentence so it is still acceptable."""),
      example(question = "What does EPA stand for?", answer = "Environmental Protection Agency", isGood = true,
              tooltip = """Asking about the meanings of words or acronyms, when they are explicitly defined
                           in the sentence, is acceptable."""),
      example(question = "What pronoun refers to the regulations?", answer = "them", isGood = false,
              tooltip = """This question is about the words in the sentence instead of the sentence's meaning,
                           so it is unacceptable."""),
      example(question = "Who enacted the regulations?", answer = "the Environmental Protection Agency (EPA)", isGood = false,
              tooltip = """This is not directly implied by the sentence, so the question is invalid.
                           (In fact, it is also wrong: it is Congress which enacts regulations, not the EPA.)"""),
      example(question = "What is Gina's last name?", answer = "McCarthy", isGood = true,
              tooltip = """This is an acceptable question much like "What does EPA stand for?",
                           but note that the similar question "What is the last word in Gina's name? would be invalid."""),
      example(question = "What is the is the Agency responsible for?", answer = "Environmental Protection", isGood = true,
              tooltip = """While "responsibility" is not explicitly mentioned in the sentence,
                           this fact is part of the meaning of the name "Environmental Protection Agency"."""),
      example(question = "Was McCarthy aggressive or lax?", answer = "aggressive", isGood = false,
              tooltip = """This is an either/or question, which is disallowed."""),
      example(question = "What was enforced?", answer = "them", isGood = true,
              tooltip = """The answer "the regulations" is also acceptable here. It is okay for the answer
                           to be non-unique if it is because multiple different phrases
                           refer to the same thing.""")
    ),
    <.p("Now consider the following sentence, with the special word ", <.b("decision. ")),
    <.blockquote(<.i("""I take full and complete responsibility for my thoughtless """, <.span(Styles.specialWord, """decision"""),
                     """ to disclose these materials to the public. """)),
    <.p("Here are examples of some good question-answer pairs:"),
    <.ul(
      <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
                 <.span("Who "), <.b("decided "), <.span("something? --> I"),
                 <.span(^.className := "tooltiptext",
                        """Where possible, change nouns like "decision" to verbs in order to write short questions about them."""))),
      <.li(<.div(Styles.goodGreen, ^.className := "tooltip",
                 <.span("What kind of "), <.b("decision"), <.span("? --> thoughtless"),
                 <.span(^.className := "tooltiptext",
                        """To get descriptive words as answers, you may need to ask "What kind" or similar questions.""")))
    ),
    <.p("""Now suppose you are given the following sentence, with the special word """, <.b("pushed. ")),
    <.blockquote(<.i("""Alex """, <.span(Styles.specialWord, "pushed"), """ Chandler at school today.""")),
    <.p("Mouse over the following examples of bad question-answer pairs for explanations:"),
    <.ul(
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Who got hurt? --> Chandler"),
                 <.span(^.className := "tooltiptext",
                        """The question must include some content word from the sentence, which this fails to do."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Did Alex or Chandler ", <.b("push"), " someone? --> Alex"),
                 <.span(^.className := "tooltiptext",
                        """Either/or and yes/no questions are not allowed."""))),
      <.li(<.div(Styles.badRed, ^.className := "tooltip",
                 <.span("Where did Alex ", <.b("push"), " Chandler? --> at school today"),
                 <.span(^.className := "tooltiptext",
                        """The question asked "where", so including the word "today" is incorrect.""")))
    ),
    <.h2("Redundancy"),
    <.p(""" Two question-answer pairs are """, <.b("redundant "), """if they are both """,
        <.b("asking the same question "), "and they ", <.b("have the same answer. "), """
        None of your question-answer pairs in one HIT should be redundant with each other.
        For example, consider the following sentence and questions:"""),
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
    <.h2("""Conditions & Bonuses"""),
    <.p(s"""For each HIT, you will be shown up to four special words from the sentence.
          You are required to write at least one question-answer pair for each special word.
          However, you will receive bonuses if you come up with more.
          (As you complete each one, new fields will appear for you to write more.)
          The bonus per question increases by ${dollarsToCents(bonusIncrement)}c for each one you write;
          your reward will be greatest if you can present """,
          <.b("the complete set of possible questions and answers "),
          """that relate the special words to each other and the rest of the sentence.
          On average, it should take less than 30 seconds per question-answer pair.
          """),
    <.p("""Your work will be evaluated by other workers according to the above criteria. """,
          <.b("""You will only be awarded bonuses for your good, non-redundant question-answer pairs, """),
          s""" as judged by other workers.
          This means the "total potential bonus" indicator is just an upper bound on what you may receive,
          which will depend on the quality of your responses.
          Your bonus will be awarded as soon as validators have checked all of your question-answer pairs,
          which will happen shortly after you submit (but will vary depending on worker availability).
          Your accuracy qualification value for this HIT will be updated to match your current accuracy
          as your questions are validated.
          If this number drops below ${(100 * generationAccuracyBlockingThreshold).toInt},
          you will no longer qualify for the task.
          There is a grace period of several HITs before your score is allowed to drop too low;
          if your score is exactly ${(100 * generationAccuracyBlockingThreshold).toInt}
          it may be that your real accuracy is lower but you are in the grace period.
          The first time your score gets near or below the threshold, you will be sent a notification,
          but you can check it at any time in your qualifications.
          (Note, however, that the validators will sometimes make mistakes,
          so there is an element of randomness to it: don't read too deeply into small changes in your accuracy.)"""),
    <.h2("""Tips"""),
    <.p(s"""To make the task go quickly, make your questions as short and simple as possible.
            (There is a ${questionCharLimit}-character limit, which will be indicated in red when you approach it.)
            Feel free to use generic words like "someone" and "something" to do so."""),
    <.p(""" You will find that the vast majority of your questions begin with """,
        <.b("Who, what, when, where, why, whose, which, "),
        " or ",
        <.b("how"),
        """. There is also variety of possible """,
        <.i(" what"), ", ", <.i(" which"), ", and ", <.i(" how "), """ questions you may ask,
        which start with phrases like """,
        <.b(" What color, what day, which country, which person, how much, how long, how often, how large, "),
        """ and many others. If you're having trouble coming up with questions using these words,
        remember that you can use the special word in """, <.b(" either the question or the answer, "),
        """ and when using it in the question, you can change its form,
        like turning "decision" into "decide", or expanding symbols to their English versions
        (like $ as dollars, or ° as degrees). """),
    <.p("""Finally, it's to your advantage to """,
        <.b("write as many good questions as possible. "),
        """ If you can come up with more questions that you're sure are valid in one HIT,
            it will help keep your accuracy high in case you have trouble writing the required questions in other HITs.
            In addition, the bonuses increase the more questions you write,
            so this will help maximize your earnings per question."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field.""")
    )
}

// <.ul(
//   <.li("Who ", <.i("verb"), "ed something?"),
//   <.li("What was ", <.i("verb"), "ed?"),
//   <.li("What is ", <.i("adjective"), "?"),
//   <.li("What kind of ", <.i("noun"), "?"),
//   <.li("Whose ", <.i("noun"), "?"),
//   // <.li("When/where was <noun>?"),
//   // <.li("How many/much ", <.i("noun"), "?"),
//   <.li("How ", <.i("adjective"), " is ", <.i("noun"), "?"),
//   <.li("Who/What/Where/When/Why/How did someone ", <.i("verb"), "?")
// ),
