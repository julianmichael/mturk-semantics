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
    <.blockquote(<.i("Local officials ", <.span(Styles.bolded, " promised "), " on Tuesday that they will ",
                     <.span(Styles.bolded, " resume "), " the investigation, after ",
                     <.span(Styles.bolded, " facing "), "heavy pressure from demonstrators.")),
    <.p("""You should write all of the following:"""),
    <.ul(
      <.li("Who ", <.span(" promised "), " something? --> Local officials / they"),
      <.li("What did someone ", <.span(" promise"), "? --> that they will resume the investigation"),
      <.li("What did someone ", <.span(" promise "), " to do? --> resume the investigation"),
      <.li("When did someone ", <.span(" promise "), " to do something? --> on Tuesday / after facing heavy pressure from demonstrators"),
      <.li("Why did someone ", <.span(" promise "), " to do something? --> heavy pressure from demonstrators"),
      <.li("Who might ", <.span(" resume "), " something? --> Local officials / they"),
      <.li("What might be ", <.span(" resumed"), "? --> the investigation"),
      <.li("Who ", <.span(" faced "), " something? --> Local officials / they"),
      <.li("What did someone ", <.span(" face"), "? --> heavy pressure from demonstrators")),
    <.p(""" See below for further explanation. """),
    <.h2("""Question Format"""),
    <.p(""" The interface will force your questions to follow a strict format, filling 7 slots as in the examples repeated below.
        This will keep your questions simple, focused, and easier to write.
        Most slots (but not the wh-word and verb) may be omitted, depending on the context.
        Also, not every way of filling the slots will be accepted by the autocomplete: some bad combinations (e.g., """, <.i(" Who has looking?"), """)
        will be ruled out automatically.
        However, you are still responsible for making sure you write grammatical questions.
        """),
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
        <.tr(<.td("Why"), <.td("did"), <.td("someone"), <.td("promise"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("Who"), <.td("might"), <.td(), <.td("resume"), <.td("something"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("might"), <.td(), <.td("be resumed"), <.td(), <.td(), <.td()),
        <.tr(<.td("Who"), <.td(), <.td(), <.td("faced"), <.td("something"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("face"), <.td(), <.td(), <.td())
      )
    ),
    <.h2("Redundancy"),
    <.p(""" Two question-answer pairs are """, <.b("redundant "), """if they are both """,
        <.b("asking the same question "), "and they ", <.b("have the same answer. "), """
        None of your question-answer pairs about a single verb may be redundant with each other.
        For example, consider the following two questions:"""),
    <.ul(
      <.li(<.div("What did someone promise?")),
      <.li(<.div("What was promised?"))
    ),
    <.p("""The second is just another way of phrasing the first: the answer to one will always be the same as the answer to the other. So """,
        <.b(Styles.badRed, "these are redundant"), """. You should ask only one of the two.
        Now consider the following:"""),
    <.ul(
      <.li(<.div("Who promised something?")),
      <.li(<.div("Who would have promised something?"))
    ),
    <.p("""Again, """, <.b(Styles.badRed, "these are redundant"), """.
        They are asking about the same thing (who is doing the "promising"), just using different tense/aspect (auxiliary verbs).
        Of these two you should only ask the one whose auxiliary verbs make the most sense with the sentence.
        Finally, consider the following: """),
    <.ul(
      <.li(<.div("What did someone promise?")),
      <.li(<.div("What did someone promise to do?"))
    ),
    <.p("""While these are asking about something very similar, they are not always interchangeable
        (consider the sentence """, <.i(" He promised he was clean"), """)
        and they have different answers (see the example at the top), so these questions are """, <.b(Styles.goodGreen, "not redundant"), "."),

    <.h2("Multiple Answers"),
    // TODO
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

    <.h2("""Tips for Writing Questions"""),
    <.p("Consider again the example sentence:"),
    <.blockquote(<.i("Local officials ", <.span(Styles.bolded, " promised "), " on Tuesday that they will ",
                     <.span(Styles.bolded, " resume "), " the investigation, after ",
                     <.span(" facing "), "heavy pressure from demonstrators.")),
    <.p(""" When trying to come up with questions for a verb, remember to consider all of the following: """),
    <.h4("Subject and Object"),
    <.p("""Almost every time, you can ask one or more questions like these:"""),
    <.ul(
      <.li(<.div("Who promised something?")),
      <.li(<.div("What did someone promise?")),
      <.li(<.div("What did someone promise to do?"))
    ),
    <.p("Don't forget these, since they're low-hanging fruit."),
    <.h4("Hypotheticals and negation"),
    <.p("""Sometimes the verb (like """, <.i("resume"), """ above) doesn't denote something actually happening.
        You should use words like """, <.i("would"), " or ", <.i("might"), """ in these cases, as in """,
        <.i("Who might resume something?"), """ In case of something that is not true, you can use the word """,
        <.i(" not "), """ or contractions like """,
        <.i("didn't"), " in your questions.",
        """ You might also have multiple kinds of question for a single verb: for example, in the sentence """
    ),
    <.blockquote("She offered to help with his homework, but not to do it for him."),
    <.p(""" You should write both """,
        <.i(" What did someone offer to do? --> help with his homework,"),
        """ and """,
        <.i(" What did someone not offer to do? --> do it for him."),
        """ You should only use "not" for things that are stated not to be the case in the sentence,
        not just things that are untrue. For example, """,
        <.i(" Who did not offer to do something? --> him"),
        """ would be unacceptable. """),
    <.h4("When, Where, Why, and How"),
    <.p("""Almost all verbs can get these questions; it just depends on whether the answer appears in the sentence.
        Sometimes (as with the """, <.i("Why"), """ example above) it can be easy to miss them,
        so it can help to quickly run through them and ask yourself for each one."""),
    <.h4("Prepositions"),
    <.p("""Some questions are best asked using certain prepositions. For example, if the sentence says """,
        <.i("Protesters blamed the disaster on unchecked deregulation"), """, you should be asking """,
        <.i("What did someone blame something on?")),
    <.h4("Someone and Something"),
    <.p("""Finally, you may notice that it's possible to include or omit placeholder words like """,
        <.i("someone"), " and ", <.i("something"), """. For example, it may seem acceptable to ask either """,
        <.i("Who promised?"), " or ", <.i("Who promised something?"),
        """ However, these will not always have the same meaning: consider """,
        <.i("What broke?"), " versus ", <.i("What broke something?"),
        """ Please default to including these placeholder words when it does not affect the meaning of the question. """),

    <.h2("""Interface Controls"""),
    <.ul(
      <.li(""" You can use the mouse, the up and down arrow keys, and the enter key to navigate the autocomplete menu.
        However, once you get used to the question format, it will most likely be fastest just to type the questions and use
        autocomplete to verify that they are correct."""),
      <.li(""" We recommend leaving your mouse hovering over the sentence and using tab and shift+tab to switch between questions
        so you don't have to spend time moving the mouse back and forth.
        (we have made it so text fields should not lose focus when you highlight in the sentence.) """),
      <.li(""" You may highlight words by clicking or by dragging.
        To erase highlights, click or start dragging on a word that is already highlighted.""")),
    <.p("""If you have any questions, concerns, or points of confusion,
        please share them in the "Feedback" field.""")
  )

  def validationExample(question: String, answer: String, isGood: Boolean, tooltip: String) =
    <.li(
      <.div(
        ^.className := "tooltip",
        <.span(question),
        <.span(" --> "),
        <.span(
          if(isGood) Styles.goodGreen else Styles.badRed,
          answer),
        <.span(^.className := "tooltiptext", tooltip)
      )
    )


  override val validationInstructions = <.div(
    <.h2("""Task Summary"""),
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions (usually at least four)
           prepared by other annotators."""),
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
    <.p(<.b("""Warning: """), """The text shown to you is drawn randomly
           from Wikipedia and news articles from the past few years.
           We have no control over the contents of the text, which may discuss sensitive subjects,
           including crime and death, or occasionally contain offensive ideas. Please use appropriate discretion."""),
    <.h2("""Requirements"""),
    <.p("""This task is best fit for native speakers of English.
        For each question, you will either """,
        <.b("answer it, "), "mark it ", <.b("invalid, "), "or mark it ", <.b("redundant.")),
    <.h3("Answers"),
    <.p("""Each answer must be """, <.b("correct "), "and ", <.b("as grammatical as possible"),
        """. Include only the words relevant for answering the question,
        but if all else is equal, prefer longer answers over shorter ones.
        If there are multiple correct answers written as a list or with an "and", you should choose the whole list."""),
    <.p("""In long sentences, an object may be mentioned multiple times, or a phrase may appear in the sentence multiple times.
           In cases such as this where there are multiple possible correct answers,
           you should choose the phrase that most naturally answers the question in your opinion.
           This may be the best phrase for describing the answer,
           or it might be the closest one to the content that the question is asking about.
           (Since this is a small minority of cases, disagreements on these
           should not significantly hurt your agreement numbers.)"""),
    <.h3("Invalid Questions"),
    <.p("""A question should be marked invalid if any of the following are true:"""),
    <.ul(
      <.li("""It isn't about the meaning of the sentence (for example, asking "Which word comes after...")."""),
      <.li("It is not fluent English or has grammatical or spelling errors."),
      <.li("It is not obviously answered by what is expressed in the sentence."),
      <.li("""It does not contain any words from the sentence (for example, "What happened?" is usually invalid). Changing the forms of words (like changing "decision" to "decide") and expanding symbols (like writing $ as dollars or ° as degrees) is fine."""),
      <.li("It is a yes/no or either/or question, or other non-open-ended question.")
    ),
    <.p("""It is okay for a question not to be a full sentence, as long as it makes sense and it is grammatical English.
           For example, the question """, <.span(Styles.goodGreen, "Whose decision?"), """ would be fine if the phrase
           "my decision" appeared in the sentence. """,
        """Note that such short questions might lack the context we normally provide in conversational speech,
           but this does not make them invalid.
           Be sure to read the entire sentence to figure out what the question writer is asking about."""),
    <.p("""Questions might include the question word like "what" in the middle somewhere,
           as in """, <.span(Styles.goodGreen, "Protesters celebrated after what form of intervention?"), """ This is fine, but
           if the question is excessively unnatural, like """, <.span(Styles.badRed, "The what protesters?"), """
           or if it lacks a question word altogether and simply copies a phrase from the sentence
           (for example, """, <.span(Styles.badRed, "The protesters celebrated after?"), """) then it should be counted invalid.
        """),
    <.p("""If a question betrays a clear misunderstanding of the task or is clearly not written by a native English speaker,
           it should be counted invalid. You should forgive minor spelling errors (e.g., who's/whose, it's/its)
           as long as they do not change the meaning of the question."""),
    <.p("""If a question is both invalid and redundant, please mark it invalid."""),
    <.h3("Redundancy"),
    <.p(""" Two questions are """, <.b("redundant "), """if they """,
        <.b("have the same meaning "), "and they ", <.b("have the same answer. "), """
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
    <.p(""" You may also come upon two questions ask about essentially the same thing,
        but where the order of question and answer is reversed. In these cases, the two are """, <.b(" not "),
        """ redundant, since they have different answers. """),
    <.h2("""Examples"""),
    <.p("Suppose you are given the following sentence:"),
    <.blockquote(<.i(""" In the year since the regulations were enacted,
                         the Director of the Environmental Protection Agency (EPA),
                         Gina McCarthy, has been aggressive in enforcing them.""")),
    <.p("""Here are examples of questions others might write, and how you should answer them.
           Mouse over each for an explanation."""),
    <.ul(
      validationExample(question = "What was enacted?", answer = "the regulations", isGood = true,
              tooltip = """This is a standard, straightforward question that is answered literally by the sentence.
                           Most questions should look something like this."""),
      validationExample(question = "In the what since?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer simply replaced a word with "what"
                           instead of using it to form a proper English question."""),
      validationExample(question = "How long was it since the regulations were enacted?", answer = "the year", isGood = true,
              tooltip = """While "a year" is a more natural answer, "the year" is the closest you can get
                           and the question is answered in the sentence so it is still acceptable."""),
      validationExample(question = "What does EPA stand for?", answer = "Environmental Protection Agency", isGood = true,
              tooltip = """Asking about the meanings of words or acronyms, when they are explicitly defined
                           in the sentence, is acceptable."""),
      validationExample(question = "What pronoun refers to the regulations?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer may have had the word "them" in mind, but this question
                           is about the words in the sentence instead of the sentence's meaning,
                           so it is unacceptable."""),
      validationExample(question = "Who enacted the regulations?", answer = "<Invalid>", isGood = false,
              tooltip = """The question writer may have been thinking it was the EPA, but that is not
                           actually expressed in the sentence, so the question is invalid.
                           (In fact, they were also wrong: it is Congress which enacts regulations, and not the EPA.)"""),
      validationExample(question = "What is Gina's last name?", answer = "McCarthy", isGood = true,
              tooltip = """This is an acceptable question much like "What does EPA stand for?",
                           but note that the similar question "What is the last word in Gina's name? would be invalid."""),
      validationExample(question = "What is the is the Agency responsible for?", answer = "Environmental Protection", isGood = true,
              tooltip = """While "responsibility" is not explicitly mentioned in the sentence,
                           this fact is part of the meaning of the name "Environmental Protection Agency".
                           Breaking down the meanings of names and descriptors like this is fine."""),
      validationExample(question = "Was McCarthy aggressive or lax?", answer = "<Invalid>", isGood = false,
              tooltip = """This is an either/or question, which is disallowed.""")
    ),
    <.p("Now suppose you are given the following sentence:"),
    <.blockquote(<.i("""I take full and complete responsibility for
                        my decision to disclose these materials to the public.""")),
    <.p("""Here are some more examples:"""),
    <.ul(
      validationExample(question = "Who decided to disclose something?", answer = "I", isGood = true,
              tooltip = """You can use pronouns like "I" and "it" to answer questions as long as they refer to the correct answer."""),
      validationExample(question = "What is someone responsible for?", answer = "my decision to disclose these materials to the public", isGood = true,
              tooltip = """If shorter and longer answers are equally correct, favor the longer one.
                           Provide this answer instead of just "my decision"."""),
      validationExample(question = "Who made the decision?",
              answer = """<Redundant with "Who decided to disclose something?">""", isGood = false,
              tooltip = """The question has the same meaning as asking who "decided" to do it,
                           as in the first question - and the answer is the same,
                           so this question is redundant."""),
      validationExample(question = "Who disclosed the materials?",
              answer = "I",
              isGood = true,
              tooltip = """This is not redundant with the first question, because it is asking about who did the disclosing
                           rather than who did the deciding."""),
      validationExample(question = "What did someone leak?",
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
