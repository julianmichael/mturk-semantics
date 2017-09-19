package example.multitask

import turksem.qasrl.QASRLSettings
import turksem.qasrl.QASRLDispatcher
import turksem.qamr._
import turksem.util._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scalajs.js.JSApp

object Dispatcher extends QASRLDispatcher[SentenceId] with JSApp {

  val dataToggle = VdomAttr("data-toggle")
  val dataPlacement = VdomAttr("data-placement")

  val TooltipsComponent = ScalaComponent.builder[VdomTag]("Tooltips")
    .render(_.props)
    .componentDidMount(_ => Callback(scala.scalajs.js.Dynamic.global.$("[data-toggle=\"tooltip\"]").tooltip()))
    .build

  import QASRLSettings._

  def generationExample(question: String, answer: String, isGood: Boolean, tooltip: String = "") =
    <.li(
      <.span(
        if(isGood) Styles.goodGreen else Styles.badRed,
        TagMod(
          Styles.underlined,
          dataToggle := "tooltip",
          dataPlacement := "top",
          ^.title := tooltip).when(tooltip.nonEmpty),
        <.span(question),
        <.span(" --> "),
        <.span(answer)
      )
    )

  private[this] val examples = TooltipsComponent(
    <.div(
      <.p(" Below, for each verb, we list a complete set of good questions (green) and some bad ones (red). ",
          " (This section is exactly the same between the question writing and question answering tasks.) ",
          " Hover the mouse over the underlined examples for an explanation. "),
      <.blockquote(
        ^.classSet1("blockquote"),
        "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today refused to promise that they would resume the investigation before year's end. "),
      <.ul(
        generationExample(
          "Who blamed someone?",
          "Protesters",
          true),
        generationExample(
          "Who did someone blame something on?",
          "local officials",
          true),
        generationExample(
          "What did someone blame someone for?",
          "the corruption scandal",
          true,
          """ "What did someone blame on someone?" would also have been okay. """),
        generationExample(
          "Who blamed?",
          "Protesters",
          false,
          """ This question is invalid by the litmus test, because the sentence "Protesters blamed." is ungrammatical. """)
      ),

      <.blockquote(
        ^.classSet1("blockquote"),
        "Protesters blamed the corruption scandal on local officials, who today ", <.span(Styles.bolded, " refused "), " to promise that they would resume the investigation before year's end. "),
      <.ul(
        generationExample(
          "Who refused to do something?",
          "local officials / they",
          true,
          """When answering, list all of the phrases in the sentence that refer to the correct answer, including pronouns like "they"."""),
        generationExample(
          "What did someone refuse to do?",
          "promise that they would resume the investigation before year's end",
          true),
        generationExample(
          "What did someone refuse to do?",
          "promise that they would resume the investigation",
          false,
          """The answer is not specific enough: it should include "before year's end" because that was part of what they were refusing to promise."""),
        generationExample(
          "What did someone refuse to do?",
          "resume the investigation before year's end",
          false,
          """This answer is also bad: you should instead choose the more literal answer above."""),
        generationExample(
          "When did someone refuse to do something?",
          "today",
          true),
        generationExample(
          "Who didn't refuse to do something?",
          "Protesters",
          false,
          """The sentence does not say anything about protesters refusing or not refusing, so this question is invalid.""")
      ),

      <.blockquote(
        ^.classSet1("blockquote"),
        "Protesters blamed the corruption scandal on local officials, who today refused to ", <.span(Styles.bolded, " promise "), " that they would resume the investigation before year's end. "),
      <.ul(
        generationExample(
          "Who didn't promise something?",
          "local officials / they",
          true,
          "Negated questions work when the sentence is indicating that the event or state expressed by the verb did not happen."),
        generationExample(
          "What didn't someone promise?",
          "that they would resume the investigation before year's end",
          true),
        generationExample(
          "When didn't someone promise to do something?",
          "before year's end",
          false,
          """ This question is bad because "before year's end" refers to the timeframe of resuming the investigation, not the timeframe of the promise being made.
            All such questions must pertain to the time/place of the chosen verb. """)
      ),

      <.blockquote(
        ^.classSet1("blockquote"),
        "Protesters blamed the corruption scandal on local officials, who today refused to promise that they would ", <.span(Styles.bolded, " resume "), " the investigation before year's end. "),
      <.ul(
        generationExample(
          "Who might resume something?",
          "local officials / they",
          true,
          """Words like "might" or "would" are appropriate when the sentence doesn't clearly indicate whether something actually happened."""),
        generationExample(
          "What might someone resume?",
          "the investigation",
          true),
        generationExample(
          "When might someone resume something?",
          "before year's end",
          true)
      ),

      <.blockquote(
        ^.classSet1("blockquote"),
        <.span(Styles.bolded, " Let"), "'s go up to the counter and ask."),
      <.ul(
        generationExample(
          "Who should someone let do something?",
          "'s",
          true,
          """Here, you should read 's as the word it stands for: "us".
            So by substituting back into the question, we get "someone should let us do something",
            which is what someone is suggesting when they say "Let's go". """),
        generationExample(
          "What should someone let us do?",
          "go up to the counter and ask",
          true,
          """It would also be acceptable to mark "go up to the counter" and "ask" as two different answers. """),
        generationExample(
          "Where should someone let someone do something?",
          "the counter",
          false,
          """Questions should only concern the targeted verb: "letting" is not happening at the counter.""")
      ),

      <.blockquote(
        ^.classSet1("blockquote"),
        "Let's ", <.span(Styles.bolded, " go "), " up to the counter and ask."),
      <.ul(
        generationExample(
          "Who should go somewhere?",
          "'s",
          true),
        generationExample(
          "Where should someone go?",
          "up to the counter",
          true,
          """Since both "up" and "to the counter", describe where they will go, they should both be included in the answer to a "where" question. """))
    )
  )

  override val generationPreTaskInstructions =
    <.p(<.span(Styles.badRed, """Please read the detailed instructions at the bottom before you begin """),
        """ so you can maximize your bonuses and avoid losing your qualification. """,
        """ To begin working on this HIT, please request the question-answer writing accuracy qualification
            and the qualification for the number of questions asked per verb.
            They are both auto-granted.
            (Task design last updated Sept 13, 2017.)
        """)

  override val generationPostTaskInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of verbs in English sentences into lists of questions and answers.
        You will be presented with a selection of English text with a verb written in bold."""),
    <.p("""You will write questions about the verb and highlight their answers in the original sentence.
        Questions are required to follow a strict format, which is enforced by autocomplete functionality in the interface. """,
        <.b(""" You will be paid according to how many questions you write. """),
        f""" For each HIT, you should provide all possible questions and all of their correct answers,
            where none of the answers overlap with each other. You are required to write at least one question for each HIT.
            and at least ${QASRLSettings.generationCoverageQuestionsPerVerbThreshold}%.1f questions per HIT on average.""",
        """ For example, the prompt below should elicit the following questions and answers: """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today refused to promise that they would resume the investigation before year's end. "),
    <.ul(
      <.li("Who blamed someone? --> Protesters"),
      <.li("Who did someone blame something on? --> local officials"),
      <.li("What did someone blame on someone? --> the corruption scandal")
    ),
    <.h2("Guidelines"),
    <.h4("Questions"),
    <.p("""Your questions must adhere to a strict format, and none of their answers may overlap with each other.
        This is all enforced by the interface, which includes an autocomplete menu with auto-suggested questions, and lets you highlight answers in the sentence.
        In addition, your questions must satisfy the following criteria: """),
    <.ol(
      <.li(<.span(Styles.bolded, """They must be grammatical."""),
           """ To determine whether a question is grammatical, the litmus test is turning it into a sentence by substituting its answer in.
           For example: """, <.span(Styles.bolded, " Who blamed? --> Protesters"), """ would be wrong, because by substituting the answer back in, we get
           """, <.span(Styles.bolded, "Protesters blamed"), """, which is not a grammatical sentence."""),
      <.li(<.span(Styles.bolded, "You must write as many questions as possible."), f""" Each HIT will require you to write at least one question,
           and across all HITs you must average at least ${QASRLSettings.generationCoverageQuestionsPerVerbThreshold}%.1f questions per verb or you will lose your qualification.
           Rewards also increase as you write more questions so it is worth your time to do so.""")),
    <.p(""" If there is more than one question or phrasing that has the same answer, just choose one of them. """),
    <.p("Occasionally, you may get a bolded word that isn't a verb, or is hard or impossible to write questions about. ",
        " In this case, please do your best to come up with one question, even if it is nonsensical. ",
        " While it will count against your accuracy, this case is rare enough that it shouldn't matter. "),
    <.h4("Answers"),
    <.p("Your answers will be phrases that you highlight in the sentence. They must satisfy all of the following criteria:"),
    <.ol(
      <.li("You must highlight every correct answer, including phrases such as pronouns that refer to the correct answer."),
      <.li("Include only the words relevant for answering the question, but if all else is equal, prefer longer answers.")),
    <.p(""" To determine if an answer is correct, again use the litmus test of substituting it back into the question.
      It should form a grammatical sentence that is explicitly true according to the sentence.
      When you are highlighting multiple answers, be sure to click a new answer slot for each one. """),
    <.p(" Please read through the examples at the bottom for a complete explanation. "),

    <.h2("""Question format"""),
    <.p(""" This section is just for reference to help you understand the format of the questions.
        They all will be formed by filling slots like in the table below.
        The set of words you may use in each slot may depend on the words you wrote in the previous slots."""),
    <.table(
      ^.classSet1("table"),
      <.thead(
        <.tr(
          <.th("Wh-word"), <.th("Auxiliary"), <.th("Subject"), <.th("Verb"), <.th("Object"), <.th("Preposition"), <.th("Misc")
        )
      ),
      <.tbody(
        <.tr(<.td("Who"), <.td(), <.td(), <.td("blamed"), <.td("someone"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("blame"), <.td("something"), <.td("on"), <.td()),
        <.tr(<.td("Who"), <.td(), <.td(), <.td("refused"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("When"), <.td("did"), <.td("someone"), <.td("refuse"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("Who"), <.td("might"), <.td(), <.td("resume"), <.td("something"), <.td(), <.td())
      )
    ),

    <.h2("""Using the Interface"""),
    <.ul(
      <.li("""You can use the mouse, the up and down arrow keys, and the enter key to navigate the autocomplete menu.
        We suggest starting off using the mouse and clicking on the autocomplete options to write your questions.
        Once you get used to the question format, it might be fastest to type all of the questions
        (with autocomplete checking your work) and then fill in the answers.
        You can use tab and shift+tab to switch between questions quickly."""),
      <.li("""Once you have written at least one question,
        the autocomplete dropdown will start proposing completions of your other questions.
        These can significantly speed up your question writing,
        though keep in mind that the suggestions will not always be grammatical or answerable.
        The suggestions are based on the structure of your previous questions, so to get the most out of them,
        write questions with more structure (e.g., "Who looked at someone?") rather than
        less (e.g., "Who looked?").
        """),
      <.li("""Highlight words for your answers by clicking or by dragging on words in the sentence.
        To erase highlights, click or start dragging on a word that is already highlighted.
        To add a new answer, just click on the open slot next to the current answer;
        and click on a previous answer to edit it.
        You will be prevented from including any words in more than one answer. """)),
    <.p("""
      When a question-answer pair is complete (the question is finished and it has at least one answer),
      its input field will turn """, <.span(
          ^.backgroundColor := "rgba(0, 255, 0, 0.3)", "green"
        ), """. If it violates the required formatting, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 0, 0, 0.3)", "red"
        ), """. If it is a repeat of a previous question, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 255, 0, 0.3)", "yellow"
        ), """. Only complete (green) question-answer pairs will count towards your requirements and bonus. """
    ),

    <.h2("""Conditions & Bonuses"""),
    <.p(s"""Each question-answer pair after the first will earn you a bonus:
          ${dollarsToCents(generationReward)}c for the second question, ${dollarsToCents(generationReward) + 1}c for the third
          then ${dollarsToCents(generationReward) + 2}c, etc.
          While at least one is required to submit the HIT,
          you will need to write more than two questions on average in order to stay qualified.
          On average, it should take less than 30 seconds per question-answer pair, and be much quicker with practice.
          """),
    <.p("""Your questions will be evaluated by other annotators, and """,
        <.b(""" you will only be awarded bonuses for your valid question-answer pairs. """),
        s""" (However, your questions-per-verb average will include invalid questions.)
          The bonus will be awarded as soon as validators have checked all of your question-answer pairs,
          which will happen shortly after you submit (but will vary depending on worker availability).
          Your accuracy qualification value for this HIT will be updated to match your current accuracy
          as your questions are validated.
          If this number drops below ${(100 * generationAccuracyBlockingThreshold).toInt},
          you will no longer qualify for the task.
          There is a grace period of several HITs before your score is allowed to drop too low;
          if your score is exactly ${(100 * generationAccuracyBlockingThreshold).toInt}
          it may be that your real accuracy is lower but you are in the grace period.
          (Note that the validators will sometimes make mistakes,
          so there is an element of randomness to it: don't read too deeply into small changes in your accuracy.)"""),

    <.p("""If you have any questions, concerns, or points of confusion,
        feel free to share them in the "Feedback" field."""),

    <.h2("Examples"),
    examples
  )

  override val validationPreTaskInstructions =
    <.p(<.span(Styles.badRed, """ Please read the detailed instructions at the bottom before you begin. """),
        """ To begin working on this HIT, please request the question answering agreement qualification
            (it is auto-granted). Also, while there may be few HITs available at any one time,
            more will be uploaded as other workers write questions for you to validate.
            (Task design last updated Sept 15, 2017; answers now may not overlap.)""")

  override val validationPostTaskInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions prepared by other annotators."""),
    <.p("""You will highlight the words in the sentence that correctly answer each question,
           as well as mark whether questions are invalid.
           For example, consider the following sentence:"""),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today ",
      <.span(Styles.bolded, " refused "), " to promise that they would resume the investigation before year's end. "),
    <.p("""You should choose all of the following answers:"""),
    <.ul(
      <.li("Who blamed someone? --> ", <.span(Styles.goodGreen, " Protesters ")),
      <.li("Who did someone blame something on? --> ", <.span(Styles.goodGreen, " local officials / they")),
      <.li("What did someone blame on someone? --> ", <.span(Styles.goodGreen, " the corruption scandal"))),
    <.p(s"""You will be paid a ${dollarsToCents(validationBonusPerQuestion)}c bonus per question after the first $validationBonusThreshold questions if there are more than $validationBonusThreshold."""),
    <.h2("""Guidelines"""),
    <.p("""This task is best fit for native speakers of English.
        For each question, you will either """,
        <.b(" answer it "), " or mark it ", <.b(" invalid"), "."),
    <.h4("Answers"),
    <.p("Your answers will be phrases that you highlight in the sentence. They must satisfy all of the following criteria:"),
    <.ol(
      <.li("You must highlight every correct answer, including phrases such as pronouns that refer to the correct answer."),
      <.li("None of your answers may overlap with each other. (This is enforced by the interface.)"),
      <.li("Include only the words relevant for answering the question, but if all else is equal, prefer longer answers.")),
    <.p(""" To determine whether an answer is correct, the litmus test is turning it into a sentence by substituting its answer in.
        For example: for the question """, <.span(Styles.bolded, " Who blamed someone?"), """, the answer """, <.span(Styles.bolded, " Protesters "), """ would be correct,
        because the sentence does imply that """, <.span(Styles.bolded, " Protesters blamed someone."), """."""),
    <.p("""All of the questions will center around some verb in the sentence, which will be bolded in the sentence.
        Use this to disambiguate in case the verb appears multiple times."""),
    <.p("""If all of the possible answers to a question have been taken by previous questions, please mark it invalid.
        The annotators writing the questions are not supposed to repeat answers."""),
    <.h4("Invalid Questions"),
    <.p("""A question should be marked invalid if either of the following are true:"""),
    <.ul(
      <.li("It is not a grammatical English question."),
      <.li("It does not have a correct answer directly expressed in the sentence.")
    ),
    <.p(""" To determine whether a question is grammatical, again use the litmus test above.
        For example: """, <.span(Styles.bolded, " Who blamed? --> Protesters"), """ would be wrong, because by substituting the answer back in, we get
        """, <.span(Styles.bolded, "Protesters blamed"), """, which is not a grammatical sentence. """),
    <.p(" Occasionally, you may get questions about words that aren't verbs or are impossible to ask questions about. Please mark these invalid. "),

    <.p(" Please read through the examples at the bottom for a complete explanation. "),

    <.h2("Interface Controls"),
    <.ul(
      <.li("Change questions using the mouse, the up and down arrow keys, or W and S."),
      <.li("Cycle between answers using the left and right arrow keys, or A and D."),
      <.li("Click the button labeled \"Invalid\" or press the space bar to toggle a question as invalid."),
      <.li(""" Highlight words for your answers by clicking or by dragging on the sentence.
        To erase highlights, click or start dragging on a word that is already highlighted.""")
    ),
    <.h2("Conditions and payment"),
    <.p(s"""You will be paid a bonus of ${dollarsToCents(validationBonusPerQuestion)}c
        for every question beyond $validationBonusThreshold, which will be paid when the assignment is approved.
        Your judgments will be cross-checked with other workers,
        and your agreement qualification value for this HIT will be updated to match your total agreement rate.
        If this number drops below ${(100 * validationAgreementBlockingThreshold).toInt}
        you will no longer qualify for the task.
        There is a grace period of several HITs before your score is allowed to drop too low;
        if your score is exactly ${(100 * validationAgreementBlockingThreshold).toInt}
        it may be that your real agreement rate is lower but you are in the grace period.
        The first time your score gets near or below the threshold, you will be sent a notification,
        but you can check it at any time in your qualifications.
        (Note, however, that other validators will sometimes make mistakes,
        so there is an element of randomness to it: don't read too deeply into small changes in your agreement rate.)
        As long as you are qualified, your work will be approved and the bonus will be paid within an hour."""),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field."""),

    <.h2("Examples"),
    examples
  )
}
