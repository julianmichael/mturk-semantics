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

  import QASRLSettings._

  def generationExample(question: String, answer: String, isGood: Boolean, tooltip: String) =
    <.li(
      <.div(
        if(isGood) Styles.goodGreen else Styles.badRed,
        VdomAttr("data-toggle") := "tooltip",
        VdomAttr("data-placement") := "top",
        ^.title := tooltip,
        <.span(question),
        <.span(" --> "),
        <.span(answer)
      )
    )

  override val generationInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of English sentences into lists of questions and answers.
        You will be presented with a selection of English text with some of its verbs written in bold."""),
    <.p("""For each verb, you will write questions about the verb and highlight their answers in the original sentence.
        Questions are required to follow a strict format, which is enforced by autocomplete functionality in the interface. """,
        <.b(""" You will be paid according to how many questions you write. """),
        """ For each verb, you should provide all possible questions (without redundancy) and all of their correct answers. """,
        """ For example, consider the sentence: """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Local officials ", <.span(Styles.bolded, " promised "), " on Tuesday that they will ",
          <.span(Styles.bolded, " resume "), " the investigation, after ",
          <.span(Styles.bolded, " facing "), "heavy pressure from demonstrators."),
    <.p("""You should write all of the following:"""),
    <.ul(
      <.li("Who ", <.span(" promised "), " something? --> Local officials / they"),
      <.li("What did someone ", <.span(" promise"), "? --> that they will resume the investigation"),
      <.li("What did someone ", <.span(" promise "), " to do? --> resume the investigation"),
      <.li("When did someone ", <.span(" promise "), " to do something? --> on Tuesday / after facing heavy pressure from demonstrators"),
      <.li("Who might ", <.span(" resume "), " something? --> Local officials / they"),
      <.li("What might be ", <.span(" resumed"), "? --> the investigation"),
      <.li("Who ", <.span(" faced "), " something? --> Local officials / they"),
      <.li("What did someone ", <.span(" face"), "? --> heavy pressure from demonstrators")),
    <.p(""" See below for further explanation. """),
    <.h2("""Writing Questions"""),
    <.p(""" The interface will force your questions to follow a strict format, filling in slots like in the table below.
        (You do not need to know this table; it is only here for clarity.)
        This will keep your questions simple and focused, so they are easier for you to write and for us to analyze.
        Note that slots can often be omitted, and not every way of filling the slots will be accepted by the autocomplete:
        some bad combinations (e.g., """, <.i(" Who did promised?"), """) are ruled out automatically.
        However, """, <.b(" you are still responsible for making sure you write grammatical questions. ")),
    <.table(
      ^.classSet1("table"),
      <.thead(
        <.tr(
          <.th("Wh-word"), <.th("Auxiliary"), <.th("Subject"), <.th("Verb"), <.th("Object"), <.th("Preposition"), <.th("Misc")
        )
      ),
      <.tbody(
        <.tr(<.td("Who"), <.td(), <.td(), <.td("promised"), <.td("something"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("promise"), <.td(), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("promise"), <.td(), <.td("to"), <.td("do")),
        <.tr(<.td("When"), <.td("did"), <.td("someone"), <.td("promise"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("Who"), <.td("might"), <.td(), <.td("resume"), <.td("something"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("might"), <.td(), <.td("be resumed"), <.td(), <.td(), <.td()),
        <.tr(<.td("Who"), <.td(), <.td(), <.td("faced"), <.td("something"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("face"), <.td(), <.td(), <.td())
      )
    ),

    <.h4("""Examples"""),
    <.p("Consider again the example sentence:"),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Local officials ", <.span(Styles.bolded, " promised "), " on Tuesday that they will ",
                     <.span(Styles.bolded, " resume "), " the investigation, after ",
                     <.span(Styles.bolded, " facing "), "heavy pressure from demonstrators."),
    <.p(""" When trying to come up with questions for a verb, remember to consider all of the following: """),
    <.h5("Subject and Object"),
    <.p("""Almost every time, you can ask one or more questions like these:"""),
    <.ul(
      <.li(<.span(Styles.goodGreen, "Who promised something?")),
      <.li(<.span(Styles.goodGreen, "What did someone promise?")),
      <.li(<.span(Styles.goodGreen, "What did someone promise to do?"))
    ),
    <.p("Don't forget these, since they're low-hanging fruit."),
    <.h5("Hypotheticals and negation"),
    <.p("""Sometimes the verb (like """, <.i("resume"), """ above) doesn't denote something actually happening.
        You should use words like """, <.i("would"), " or ", <.i("might"), """ in these cases, as in """,
        <.span(Styles.goodGreen, "Who might resume something?"), """ In case of something that is not true, you can use the word """,
        <.i(" not "), """ or contractions like """,
        <.i("didn't"), " in your questions.",
        """ You might also have multiple kinds of question for a single verb: for example, in the sentence """
    ),
    <.blockquote(
      ^.classSet1("blockquote"),
      "She offered to help with his homework, but not to do it for him."),
    <.p("""You should write both of the following: """),
    <.ul(
      <.li(<.span(Styles.goodGreen, "What did someone offer to do? --> help with his homework")),
      <.li(<.span(Styles.goodGreen, "What did someone not offer to do? --> do it for him"))
    ),
    <.p("""You should only use "not" for things that are stated not to be the case in the sentence,
        not just things that are untrue. For example, """),
    <.ul(
      <.li(<.span(Styles.badRed, "Who did not offer to do something? --> him"))
    ),
    <.p("""would be unacceptable. """),
    <.h5("When, Where, Why, and How"),
    <.p("""Almost all verbs can get these questions; it just depends on whether the answer appears in the sentence.
        Sometimes it is easy to miss them, so it can help to quickly run through them and ask yourself for each one."""),
    <.h5("Prepositions"),
    <.p("""Some questions are best asked using certain prepositions. For example, for sentence """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters blamed the disaster on unchecked deregulation"
    ),
    <.p("""you should be asking """),
    <.ul(
      <.li(<.span(Styles.goodGreen, "What did someone blame something on?"))
    ),
    <.h5("Someone and Something"),
    <.p("""Finally, you may notice that it's possible to include or omit placeholder words like """,
        <.i("someone"), " and ", <.i("something"), """. For example, it may seem acceptable to ask either """,
        <.span(Styles.badRed, "Who promised?"), " or ", <.span(Styles.goodGreen, "Who promised something?"),
        """ However, these will not always have the same meaning: consider """,
        <.span(Styles.goodGreen, "What broke?"), " versus ", <.span(Styles.goodGreen, "What broke something?"),
        " Please ", <.b(" include "),  """ these placeholder words when it does not affect the meaning of the question. """),

    <.h2("Redundancy"),
    <.p(""" Two question-answer pairs are """, <.b("redundant "), """if they are both """,
        <.b("asking the same question "), "and they ", <.b("have the same answer. "), """
        None of your question-answer pairs about a single verb may be redundant with each other.
        For example, suppose you have already written the question """,
        <.span(Styles.goodGreen, "What did someone promise?"), "."),
    <.ul(
      <.li(<.span(Styles.badRed, "What was promised?"), """ — this is redundant,
           because it's just another way of phrasing the first, which will always have the same answer. """),
      <.li(<.span(Styles.badRed, "What would someone have promised?"), """ — you should consider this redundant as well.
           It is asking about the same thing (the claim being promised), just using different auxiliary verbs
           ("would have" versus "did"). Of this and the above, you shuold only ask the one whose auxiliary verbs
           make the most sense with the sentence.
           """),
      <.li(<.span(Styles.goodGreen, "What did someone promise to do?"), """ — this is not redundant.
           While the two questions are asking about something very similar, they are not always interchangeable
           (consider the sentence "He promised that he was clean") and they have different answers (see the example at the top).""")
    ),

    <.h2("Choosing Answers"),
    <.p(""" You must choose all correct answers to each question.
        Each answer must be """, <.b("correct "), "and ", <.b("as grammatical as possible"),
        """. Include only the words relevant for answering the question,
        but if all else is equal, prefer longer answers over shorter ones.
        If there are multiple correct answers written as a list or with an "and", you should choose the whole list.
        To determine if an answer is good, the litmus test is whether you can substitute it back into the question
        and come to a true statement expressed by the sentence.
        For example:
        """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "John was born in Alaska, and so was Mary."
    ),
    <.ul(
      <.li(Styles.goodGreen, "Who was born somewhere? --> John / Mary")
    ),
    <.p(""" Both are good answers because the sentence expresses both that """,
        <.span(Styles.goodGreen, "John was born somewhere"), " and ", <.span(Styles.goodGreen, " Mary was born somewhere."),
        """ You should also choose all answers, including pronouns, that refer to the correct answer, as in: """),
    <.ul(
      <.li(Styles.goodGreen, "Who promised something? --> Local officials / they")
    ),
    <.p("""
      If the answers appear together, prefer to highlight the entire phrase encompassing both, as in the following:
      """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Mary and John met in front of the house."
    ),
    <.ul(
      <.li(Styles.goodGreen, "Who met? --> Mary and John"),
      <.li(Styles.goodGreen, "Who met someone? --> Mary / John")
    ),
    <.p(
      """ Notice that you can ask both of these (and they are not redundant, since they have different answers).
      The answers make sense according to the litmus test above: it's true that """,
      <.span(Styles.goodGreen, " Mary and John met, "), " but not that ",
      <.span(Styles.badRed, " Mary and John met ", <.i(" someone ")),
      """ (which implies the existence of some other person who they both met). But it is true that """,
      <.span(Styles.goodGreen, " Mary met someone "), " (namely, John), and that ",
      <.span(Styles.goodGreen, " John met someone "), "(namely, Mary)."),

    <.h2("""Using the Interface"""),
    <.ul(
      <.li(""" You can use the mouse, the up and down arrow keys, and the enter key to navigate the autocomplete menu.
        We suggest starting off using the mouse and clicking on the autocomplete options to write your questions.
        Once you get used to the question format, it might be fastest to type all of the questions
        (with autocomplete checking your work) and then fill in the answers.
        You can use tab and shift+tab to switch between questions quickly."""),
      <.li(""" You may highlight words for your answers by clicking or by dragging on any of the copies of the sentence.
        To erase highlights, click or start dragging on a word that is already highlighted.
        To add a new answer, just click on the open slot next to the current answer;
        and click on a previous answer to edit it.""")),
    <.p("""
      When a question-answer pair is complete (the question is finished and it has at least one answer),
      its input field will turn """, <.span(
          ^.backgroundColor := "rgba(0, 255, 0, 0.3)", "green"
        ), """. If it violates the required formatting, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 0, 0, 0.3)", "red"
        ), """. If it is a repeat of a previous question for that verb, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 255, 0, 0.3)", "yellow"
        ), """. Only complete (green) question-answer pairs will count towards your requirements and bonus. """
    ),
    <.p("""
      The sentence is repeated before each verb so you won't have to scroll up and down.
      You may highlight your answer in any of the copies of the sentence, and it will register that answer for the question
      that is currently in focus.
      """),

    <.h2("""Conditions & Bonuses"""),
    <.p(s"""For each HIT, you will be shown a sentence with several verbs highlighted.
          You are required to write at least one question-answer pair for each verb.
          Please write as many different question-answer pairs as possible.
          After your first (or third, if this is the version of the HIT with at least three verbs),
          then each successive question-answer pair will earn you a ${dollarsToCents(bonusPerQuestion)}c bonus.
          On average, it should take less than 30 seconds per question-answer pair,
          and with some practice you should be able to go much quicker.
          """),
    <.p("""Your work will be evaluated by other workers according to criteria described in these instructions. """,
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

    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field.""")
  )

  override val validationInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions prepared by other annotators."""),
    <.p("""You will highlight the words in the sentence that correctly answer each question,
           as well as mark whether questions are invalid or redundant.
           For example, for the following sentence and questions, you should respond with the answers below:"""),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Local officials ", <.span(Styles.bolded, " promised "), " on Tuesday that they will ",
          <.span(Styles.bolded, " resume "), " the investigation, after ",
          <.span(Styles.bolded, " facing "), "heavy pressure from demonstrators."),
    <.p("""You should write all of the following:"""),
    <.ul(
      <.li("Who ", <.span(" promised "), " something? --> ", <.span(Styles.goodGreen, "Local officials / they")),
      <.li("What did someone ", <.span(" promise"), "? --> ", <.span(Styles.goodGreen, "that they will resume the investigation")),
      <.li("What did someone ", <.span(" promise "), " to do? --> ", <.span(Styles.goodGreen, "resume the investigation")),
      <.li("When did someone ", <.span(" promise "), " to do something? --> ", <.span(Styles.goodGreen, "on Tuesday / after facing heavy pressure from demonstrators")),
      <.li("Who might ", <.span(" resume "), " something? --> ", <.span(Styles.goodGreen, "Local officials / they")),
      <.li("What might be ", <.span(" resumed"), "? --> ", <.span(Styles.goodGreen, "the investigation")),
      <.li("Who ", <.span(" faced "), " something? --> ", <.span(Styles.goodGreen, "Local officials / they")),
      <.li("What did someone ", <.span(" face"), "? --> ", <.span(Styles.goodGreen, "heavy pressure from demonstrators"))),
    <.p(s"""You will be paid in accordance with the number of questions shown to you, with a bonus of
            ${dollarsToCents(validationBonusPerQuestion)}c per question after the first four
            that will be paid when the assignment is approved."""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        For each question, you will either """,
        <.b("answer it, "), "mark it ", <.b("invalid, "), "or mark it ", <.b("redundant.")),
    <.h3("Answers"),
    <.p(""" You must choose all correct answers to each question.
        Each answer must be """, <.b("correct "), "and ", <.b("as grammatical as possible"),
        """. Include only the words relevant for answering the question,
        but if all else is equal, prefer longer answers over shorter ones.
        If there are multiple correct answers written as a list or with an "and", you should choose the whole list.
        To determine if an answer is good, the litmus test is whether you can substitute it back into the question
        and come to a true statement expressed by the sentence.
        For example:
        """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "John was born in Alaska, and so was Mary."
    ),
    <.ul(
      <.li("Who was born somewhere? --> ", <.span(Styles.goodGreen, "John / Mary"))
    ),
    <.p(""" Both are good answers because the sentence expresses both that """,
        <.span(Styles.goodGreen, "John was born somewhere"), " and ", <.span(Styles.goodGreen, " Mary was born somewhere."),
        """ You should also choose all answers, including pronouns, that refer to the correct answer, as in: """),
    <.ul(
      <.li("Who promised something? --> ", <.span(Styles.goodGreen, "Local officials / they"))
    ),
    <.p(""" Each question centers around some verb in the sentence. While that question is selected,
        the corresponding verb in the sentence will be bolded. Use this to disambiguate in the case of sentences with
        verbs that appear multiple times."""),
    <.h3("Invalid Questions"),
    <.p("""A question should be marked invalid if either of the following are true:"""),
    <.ul(
      <.li("It is not a grammatical English question."),
      <.li("It does not have a correct answer expressed in the sentence..")
    ),
    <.p("""If a question is both invalid and redundant, please mark it invalid."""),
    <.h3("Redundancy"),
    <.p(""" Two question-answer pairs are """, <.b("redundant "), """if they are both """,
        <.b("asking the same question "), "and they ", <.b("have the same answer. "), """
        None of your question-answer pairs about a single verb may be redundant with each other.
        For example, suppose you have already answered the question """,
        <.span(Styles.goodGreen, "What did someone promise?"), "."),
    <.ul(
      <.li(<.span(Styles.badRed, "What was promised?"), """ — this is redundant,
           because it's just another way of phrasing the first, which will always have the same answer. """),
      <.li(<.span(Styles.badRed, "What would someone have promised?"), """ — you should consider this redundant as well.
           It is asking about the same thing (the claim being promised), just using different auxiliary verbs
           ("would have" versus "did").
           """),
      <.li(<.span(Styles.goodGreen, "What did someone promise to do?"), """ — this is not redundant.
           While the two questions are asking about something very similar, they are not always interchangeable
           (consider the sentence "He promised that he was clean") and they have different answers (see the example at the top).""")
    ),
    <.p(""" Remember that even if two questions are the same, if they align to different instances of a verb in the sentence,
        they should not be counted redundant. For example:
        """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "I ate", <.sub("1"), " one half and she ate", <.sub("2"), " the other."
    ),
    <.ul(
      <.li("Who ate", <.sub("1"), " something? --> ", <.span(Styles.goodGreen, "I")),
      <.li("Who ate", <.sub("2"), " something? --> ", <.span(Styles.goodGreen, "she"))
    ),
    <.p("(The subscripts here will not appear in the interface; rather, the corresponding word will be bolded in the sentence.)"),
    <.h2("Interface Controls"),
    <.ul(
      <.li("Change questions using the up and down arrow keys, or W and S."),
      <.li("Cycle between answers using the left and right arrow keys, or A and D."),
      <.li("Click the button labeled \"Invalid\" or press the space bar to toggle a question as invalid."),
      <.li("""To mark the selected question as redundant, just click the question it's redundant with (which will turn orange).
              To unmark it, click the orange question again."""),
      <.li("""You can only mark a question as redundant with a question that has an answer.
              (If more than two questions are mutually redundant, answer one of them and mark the others as redundant with that one.)"""),
      <.li(""" Highlight words for your answers by clicking or by dragging on the sentence.
        To erase highlights, click or start dragging on a word that is already highlighted.""")
    ),
    <.h2("Conditions and payment"),
    <.p(s"""You will be paid a bonus of ${dollarsToCents(validationBonusPerQuestion)}c
        for every question beyond $validationBonusThreshold.
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
        please share them in the "Feedback" field.""")
  )
}
