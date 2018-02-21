package example.tqa

import spacro.ui._

import qasrl.crowd.QASRLEvaluationDispatcher
import turksem.util._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scalajs.js.JSApp

import upickle.default._

object Dispatcher extends QASRLEvaluationDispatcher[SentenceId] with JSApp {

  val dataToggle = VdomAttr("data-toggle")
  val dataPlacement = VdomAttr("data-placement")

  val TooltipsComponent = ScalaComponent.builder[VdomTag]("Tooltips")
    .render(_.props)
    .componentDidMount(_ =>
    Callback {
      scala.util.Try {
        scala.scalajs.js.Dynamic.global.$("[data-toggle=\"tooltip\"]").tooltip()
      }
      ()
    }
  ).build

  import settings._

  def example(question: String, answerOpt: Option[String], tooltip: String = "") =
    <.li(
      <.span(
        if(answerOpt.nonEmpty) Styles.goodGreen else Styles.badRed,
        TagMod(
          Styles.underlined,
          dataToggle := "tooltip",
          dataPlacement := "top",
          ^.title := tooltip).when(tooltip.nonEmpty),
        <.span(question),
        answerOpt.map(answer =>
          <.span(s" --> $answer")
        ).whenDefined
      )
    )

  private[this] val examples = <.div(
    TooltipsComponent(
      <.div(
        <.p(" Below, for each verb, we list example good questions (green) and bad ones (red). ",
            " Hover the mouse over the underlined examples for an explanation. "),
        <.blockquote(
          ^.classSet1("blockquote"),
          "Most geologists become specialists in one area."),
        <.ul(
          example(
            "What does someone become?",
            Some("specialists in one area")
          ),
          example(
            "Where does someone become something?",
            None,
            """ Violates verb-relevance: "in one area" describes the specialization, not where they become specialists. """
          )
        ),
        <.blockquote(
          ^.classSet1("blockquote"),
          "Protesters blamed the corruption scandal on local officials."),
        <.ul(
          example(
            "Who blamed?",
            None,
            """ Fails the litmus test: the sentence "Protesters blamed." is ungrammatical. """)
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "The officials refused to promise that they would investigate."),
        <.ul(
          example(
            "Who refused to do something?",
            Some("The officials / they"),
            """ List all correct answers, including pronouns like "they". """),
          example(
            "What did someone refuse to do?",
            Some("promise that they would investigate"),
            """ Make sure to include "promise" since it's the literal answer. """),
          example(
            "Who didn't promise something?",
            Some("The officials / they")),
          example(
            "Who promised something?",
            None,
            """They refused to promise, which means they didn't promise, so this question isn't answerable."""),
          example(
            "Who might investigate?",
            Some("The officials / they"),
            """Words like "might" or "would" work where the event isn't explicitly happening or not.""")
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Allen arranged for Joan to lead the charge."),
        <.ul(
          example(
            "Who did someone arrange for?",
            None),
          example(
            "What did someone arrange for?",
            Some("Joan to lead the charge")),
          example(
            "What did someone arrange for someone to do?",
            Some("lead the charge"))
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Their device was intended to be a game-changer."),
        <.ul(
          example(
            "Who intended something?",
            None),
          example(
            "What was intended?",
            None),
          example(
            "What was intended to do something?",
            Some("Their device")),
          example(
            "What was something intended to do?",
            Some("be a game-changer"))
        )
      )
    )
  )

  import InstructionsComponent._


  val validationOverview = <.div(
    <.p(Styles.badRed, """Read through all of the instructions and make sure you understand the interface controls before beginning. A full understanding of the requirements will help maximize your agreement with other workers so you can retain your qualification."""),
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions (usually just one) generated by our system."""),
    <.p("""You will highlight the words in the sentence that correctly answer each question,
           as well as mark whether questions are invalid.""",
      <.b(""" Note: it takes exactly 2 clicks to highlight each answer; see the Controls tab for details. """),
        """For example, consider the following sentence:"""),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters blamed the corruption scandal on local officials."),
    <.p("""You should choose all of the following answers:"""),
    <.ul(
      <.li("Who blamed someone? --> ", <.span(Styles.goodGreen, " Protesters ")),
      <.li("Who did someone blame something on? --> ", <.span(Styles.goodGreen, " local officials / they")),
      <.li("What did someone blame on someone? --> ", <.span(Styles.goodGreen, " the corruption scandal"))),
    <.p(s"""You will be paid a ${dollarsToCents(validationBonusPerQuestion)}c bonus per question writing fourth question if there are more than four."""),
    <.h2("""Guidelines"""),
    <.ol(
      <.li(
        <.span(Styles.bolded, "Correctness. "),
        """Each answer must satisfy the litmus test that if you substitute it back into the question,
           the result is a grammatical statement, and it is true according to the sentence given. For example, """,
        <.span(Styles.bolded, "Who blamed someone? --> Protesters"), """ becomes """,
        <.span(Styles.goodGreen, "Protesters blamed someone, "), """ which is valid, while """,
        <.span(Styles.bolded, "Who blamed? --> Protesters"), """ would become """,
        <.span(Styles.badRed, "Protesters blamed, "), s""" which is ungrammatical, so it is invalid.
           Your responses will be compared to other annotators, and you must agree with them
           ${(100.0 * validationAgreementBlockingThreshold).toInt}% of the time in order to remain qualified. """),
      <.li(
        <.span(Styles.bolded, "Verb-relevance. "),
        """ Answers to the questions must pertain to the participants, time, place, reason, etc., of """,
        <.span(Styles.bolded, " the target verb in the sentence, "),
        " which is bolded and colored blue in the interface. ",
        """ For example, if the sentence is """,
        <.span(Styles.bolded,
               " He ",
               <.span(Styles.niceBlue, Styles.underlined, "promised"),
               " to come tomorrow "),
        """ and the question is """,
        <.span(Styles.badRed, " When did someone promise to do something? "),
        """ you must mark it """,
        <.span(Styles.badRed, " Invalid "),
        """ because the time mentioned, """, <.i(" tomorrow, "), " is ", <.i(" not "),
        " the time that he made the promise, but rather the time that he might come."),
      <.li(
        <.span(Styles.bolded, "Exhaustiveness. "),
        s"""You must provide every possible answer to each question.
           When highlighting answers, please only include the necessary words to provide a complete, grammatical answer,
           but if all else is equal, prefer to use longer answers.
           Also please include pronouns in the sentence that refer an answer you've already given."""
      )
    ),
    <.p(" All ungrammatical questions should be counted invalid. However, ",
        " If the sentence has grammatical errors or is not a complete sentence, please answer ",
        " questions according to the sentence's meaning to the best of your ability. "),
    <.p("Please read through the examples if you need more details.")
  )

  val validationControls = <.div(
    <.ul(
      <.li(
        <.span(Styles.bolded, "Navigation. "),
        "Change questions using the mouse, the up and down arrow keys, or W and S."),
      <.li(
        <.span(Styles.bolded, "Invalid Questions. "),
        "Click the button labeled \"Invalid\" or press the space bar to toggle a question as invalid."),
      <.li(
        <.span(Styles.bolded, "Answers. "),
        "To highlight an answer, first click on the first word in the answer, which will turn ",
        <.span(^.backgroundColor := "#FF8000", "orange"),
        ". Then click on the last word in the answer (which may be the same word) and the whole phrase will turn ",
        <.span(^.backgroundColor := "#FFFF00", "yellow"),
        ". You can highlight multiple answers to the same question in this way. ",
        " To delete an answer, click on a word in that answer while it is highlighted yellow. ")
    )
  )

  val validationConditions = <.div(
    <.p(s"""You will be paid a bonus of ${dollarsToCents(validationBonusPerQuestion)}c
        for every question beyond $validationBonusThreshold, which will be paid when the assignment is approved.
        Your judgments will be cross-checked with other workers,
        and your agreement rate will be shown to you in the interface.
        You must agree with the majority of 3 workers ${(100 * validationAgreementBlockingThreshold).toInt}
        of the time, or you will no longer qualify for the task.
        (Note that other validators will sometimes make mistakes,
        so there is an element of randomness to it: don't read too deeply into small changes in your agreement rate.)
        Your work will be approved and the bonus will be paid within an hour.""")
  )

  override val evaluationInstructions = <.div(
    Instructions(
      InstructionsProps(
        instructionsId = "instructions",
        collapseCookieId = "validationCollapseCookie",
        tabs = List(
          "Overview" -> validationOverview,
          "Controls" -> validationControls,
          "Conditions & Payment" -> validationConditions,
          "Examples" -> examples
        )
      )
    )
  )
}
