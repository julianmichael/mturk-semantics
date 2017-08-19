package turksem.qasrl

import cats.implicits._

import turksem._

import turksem.qamr._
import turksem.util._
import turkey.tasks._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw._
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.function.all._
// import monocle.std.list._
import monocle.syntax._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

class QASRLGenerationClient[SID : Reader : Writer](
  instructions: VdomTag,
  settings: PipelineSettings)(
  implicit promptReader: Reader[GenerationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[WordedQAPair]] // same as above
) extends TaskClient[GenerationPrompt[SID], List[WordedQAPair]] {

  import settings._

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[QASRLGenerationApiRequest[SID], QASRLGenerationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int)] // keyword index AMONG KEYWORDS, qa index, answer word index
  import HighlightingComponent._

  val IntState = new LocalStateComponent[Int]

  val ReferenceComponent = new ReferenceComponent[dom.html.Element]
  import ReferenceComponent._

  // import DropdownComponent._

  def emptyQA(keyword: Int) = WordedQAPair(keyword, "", Set.empty[Int])

  @Lenses case class QAGroup(
    verbIndex: Int,
    template: QASRLTemplate,
    qas: List[WordedQAPair]) {
    def withNewEmptyQA: QAGroup = copy(qas = this.qas :+ emptyQA(verbIndex))
  }

  @Lenses case class State(
    qaGroups: List[QAGroup],
    curFocus: Option[(Int, Int)]) {
  }
  object State {
    val empty: State = State(Nil, None)
    def initFromResponse(response: QASRLGenerationApiResponse): State = response match {
      case QASRLGenerationApiResponse(sentence, indicesWithTemplates) =>
        val qaGroups = indicesWithTemplates.map {
          case IndexWithTemplate(verbIndex, template) =>
            QAGroup(verbIndex, template, Nil)
        }
        State(qaGroups, None)
    }
  }

  def questionLens(groupIndex: Int, qaIndex: Int) = State.qaGroups
    .composeOptional(index(groupIndex))
    .composeLens(QAGroup.qas)
    .composeOptional(index(qaIndex))
    .composeLens(WordedQAPair.question)

  def isComplete(
    template: QASRLTemplate,
    wqa: WordedQAPair
  ): Boolean = {
    !wqa.question.isEmpty && !wqa.answer.isEmpty && {
      val lowerTokens = wqa.question.split(" ").toList.map(_.lowerCase)
      template.isValid(lowerTokens)
    }
  }

  def getCompleteQAPairs(group: QAGroup): List[WordedQAPair] = for {
    qa <- group.qas
    if isComplete(group.template, qa)
  } yield qa

  def getAllCompleteQAPairs(groups: List[QAGroup]): List[WordedQAPair] =
    groups.flatMap(getCompleteQAPairs)

  // better: a PTraversal that doesn't ask for the index back. would fix the issue of the iso being bad
  def indexingIso[A] = Iso[List[A], List[(A, Int)]](_.zipWithIndex)(_.map(_._1))
  def eachIndexed[A] = indexingIso[A].composeTraversal(each)

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def addQAFields: (State => State) = State.qaGroups.modify(groups =>
      groups.map { group =>
        if(group.qas.forall(isComplete(group.template, _))) group.withNewEmptyQA
        else group
      }
    )

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.composeTraversal(eachIndexed).modify {
          case (group, groupIndex) =>
            QAGroup.qas.composeTraversal(eachIndexed).modify {
              case (wqa, qaIndex) =>
                WordedQAPair.answer.set(
                  hs.span.collect {
                    case (`groupIndex`, `qaIndex`, aIndex) => aIndex
                  }.toSet)(wqa) -> qaIndex
            }(group) -> groupIndex
        } andThen addQAFields
      )

    def updateResponse: Callback = scope.state.map { st =>
      setResponse(getAllCompleteQAPairs(st.qaGroups))
    }

    def qaField(s: State, sentence: Vector[String], groupIndex: Int, qaIndex: Int) = s match {
      case State(qaGroups, curFocus) =>
        val qaGroup = qaGroups(groupIndex)
        val template = qaGroup.template
        val isFocused = curFocus == Some(groupIndex, qaIndex)
        val numQAsInGroup = qaGroup.qas.size
        val WordedQAPair(_, question, answer) = qaGroup.qas(qaIndex)
        val isAnswerEmpty = answer.isEmpty
        val nextBonus = bonusFor(getAllCompleteQAPairs(qaGroups).size + 1)

        // TODO make question templater work on arb. strings
        // and do the tokenization itself (also track bad character index?),
        val questionTokens = question.split("\\s+|\\(?=\\?\\)|\\(?<=\\?\\)").toVector
        // ^ splits on spaces and splits out q-mark. TODO maybe remove lookbehind, since apparently it doesn't work (and we don't need)

        case class TokenSuggestion(token: String, isPrefixCompletion: Boolean)

        sealed trait AutocompleteState
        case object Complete extends AutocompleteState
        @Lenses case class NextWords(menuItems: List[TokenSuggestion]) extends AutocompleteState

        object AutocompleteState {
          def nextWords: Prism[AutocompleteState, NextWords] = GenPrism[AutocompleteState, NextWords]
        }

        val menuItemsPrism = AutocompleteState.nextWords composeLens NextWords.menuItems

        val wordIncompleteTemplateState = template.processTokens(questionTokens.init.map(_.lowerCase))
        val (autocompleteState, invalidTokenIndexOpt) = wordIncompleteTemplateState match {
          case template.Invalid(nextStates, firstInvalidTokenIndex) =>
            val menuItems = template
              .getNextSlotChoices(nextStates)
              .flatMap(_.tokens)
              .toList.sortBy(_.toLowerCase)
              .map(TokenSuggestion(_, true))
            (NextWords(menuItems), Some(firstInvalidTokenIndex))
          // cannot add anything after completing (we know this due to q-mark)
          case template.Complete =>
            (NextWords(Nil), Some(questionTokens.size - 1))
          case template.InProgress(nextStates) =>
            val nextSlotChoices = template.getNextSlotChoices(nextStates)
            val lastToken = questionTokens.last.lowerCase
            // can either use next word as prefix for next state, or as complete word
            val lastTokenCompletions = nextSlotChoices
              .flatMap(_.prefixMatches(lastToken))
              .filter(_.lowerCase != lastToken)
              .map(TokenSuggestion(_, true))
              .toSet
            val nextTokenState = template.advanceStateThroughToken(
              wordIncompleteTemplateState,
              questionTokens.last.lowerCase,
              questionTokens.size - 1)

            nextTokenState match {
              case template.Complete => (Complete, None)
              case template.InProgress(nextStates) =>
                val newItems = template
                  .getNextSlotChoices(nextStates)
                  .flatMap(_.tokens)
                  .map(TokenSuggestion(_, false))
                val allItems = lastTokenCompletions ++ newItems
                val menuItems = allItems.toList.sortBy(_.token.toLowerCase)
                (NextWords(menuItems), None)
              case template.Invalid(_, firstInvalidTokenIndex) =>
                val menuItems = lastTokenCompletions.toList.sortBy(_.token.toLowerCase)
                val invalidTokenIndexOpt = Some(firstInvalidTokenIndex)
                  .filter(_ => lastTokenCompletions.isEmpty)
                (NextWords(menuItems), invalidTokenIndexOpt)
            }
        }

        // TODO fix formatting problems:
        // 1. place autocomplete menu in the right place (likely requires splitting it out into its own component)
        // 2. fix issues with focus; keep text field focused even when interacting with menu
        <.div(
          ^.display := "inline-block",
          ^.overflow := "hidden",
          IntState.LocalState(
            IntState.LocalStateProps(
              initialValue = 0,
              render = (highlightedIndex: Int, setHighlightedIndex: Int => Callback) => {
                def selectItem(suggestion: TokenSuggestion): Callback = suggestion match {
                  case TokenSuggestion(token, isPrefixCompletion) =>
                    // TODO handle when have invalid token
                    val beforeToken = if(isPrefixCompletion) questionTokens.init else questionTokens
                    scope.modState(
                      questionLens(groupIndex, qaIndex).set(
                        (beforeToken :+ token).mkString(" ")
                      ))
                }
                def handleKey(menuItems: List[TokenSuggestion])(e: ReactKeyboardEvent): Callback = {
                  def next = setHighlightedIndex((highlightedIndex + 1) % menuItems.size)
                  def prev = setHighlightedIndex((highlightedIndex - 1 + menuItems.size) % menuItems.size)
                  CallbackOption.keyCodeSwitch(e) {
                    case KeyCode.Down => next
                    case KeyCode.Up => prev
                    case KeyCode.Enter => selectItem(menuItems(highlightedIndex))
                  } >> e.preventDefaultCB
                }

                <.div(
                  invalidTokenIndexOpt.map(invalidTokenIndex =>
                    <.div() // TODO red at badness location if bad
                  ).whenDefined,
                  Reference(
                    ReferenceProps(
                      referencedTag = <.input(
                        (^.disabled := true).when(isNotAssigned),
                        (^.backgroundColor := "#88FF88").when(autocompleteState == Complete),
                        ^.float := "left",
                        ^.`type` := "text",
                        ^.placeholder := (
                          if(qaIndex == 0) "Question (required)"
                          else s"Question (+${math.round(100 * nextBonus).toInt}c)"
                        ),
                        ^.margin := "1px",
                        ^.padding := "1px",
                        ^.width := "480px",
                        menuItemsPrism.getOption(autocompleteState).map(menuItems =>
                          ^.onKeyDown ==> handleKey(menuItems)
                        ).whenDefined,
                        ^.onChange ==> (
                          (e: ReactEventFromInput) => {
                            val newValue = e.target.value
                            scope.modState(questionLens(groupIndex, qaIndex).set(newValue) andThen addQAFields)
                          }),
                        ^.onFocus --> scope.modState(State.curFocus.set(Some((groupIndex, qaIndex)))),
                        ^.onBlur --> scope.modState(State.curFocus.set(None)),
                        ^.value := question
                      ),
                      render = {
                        case (input, refOpt) =>
                          val rectOpt = refOpt.map(_.getBoundingClientRect)
                            <.div(
                              input,
                              rectOpt.flatMap(rect =>
                                AutocompleteState.nextWords.getOption(autocompleteState).map {
                                  case NextWords(menuItems) =>
                                    <.div(
                                      ^.position := "absolute",
                                      ^.top := s"${math.round(rect.bottom)}px",
                                      ^.left := s"${math.round(rect.left)}px",
                                      ^.minWidth := s"${math.round(rect.width)}px",
                                      ^.border := "solid 1px #ccc",
                                      ^.borderRadius := "3px",
                                      ^.boxShadow := "0 2px 12px rgba(0, 0, 0, 0.1)",
                                      ^.background := "rgba(255, 255, 255, 0.9)",
                                      ^.padding := "2px 0",
                                      ^.fontSize := "90%",
                                      ^.overflow := "auto",
                                      ^.maxHeight := "50%",
                                      menuItems.zipWithIndex.toVdomArray {
                                        case (ts @ TokenSuggestion(token, isPrefixCompletion), tokenIndex) =>
                                          <.div(
                                            (^.backgroundColor := "rgb(40, 162, 254)").when(tokenIndex == highlightedIndex),
                                            ^.padding := "2px 6px",
                                            ^.cursor := "default",
                                            ^.onMouseEnter --> setHighlightedIndex(tokenIndex),
                                            ^.onClick --> selectItem(ts),
                                            token
                                          )
                                      }
                                    )
                                }
                              ).whenDefined.when(isFocused)
                            )
                      }
                    )
                  )
                )
              })),
          <.div(
            Styles.answerIndicator,
            ^.float := "left",
            ^.minHeight := "1px",
            ^.width := "25px",
            "-->".when(isFocused)
          ),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            (^.color := "#CCCCCC").when(isAnswerEmpty),
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
          request = prompt,
          onLoad = ((response: QASRLGenerationApiResponse) => scope.setState(State.initFromResponse(response))),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(QASRLGenerationApiResponse(sentence, _), _) =>
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateHighlights, render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(setSpan, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val curCompleteQAPairs = getAllCompleteQAPairs(s.qaGroups)

                      val curPotentialBonus = (1 to (curCompleteQAPairs.size - s.qaGroups.size))
                        .map(bonusFor).sum

                      val curAnswer = s.curFocus.fold(Set.empty[Int]) {
                        case (groupIndex, qaIndex) => spans.collect {
                          case (`groupIndex`, `qaIndex`, ansIndex) => ansIndex
                        }
                      }

                      def touchWord(i: Int) = s.curFocus.fold(Callback.empty) {
                        case (groupIndex, qaIndex) => touchElement((groupIndex, qaIndex, i))
                      }

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
                            sentence.indices.toList,
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
                                Styles.specialWord.when(prompt.keywords.contains(index)),
                                Styles.niceBlue.when(prompt.keywords.contains(index)),
                                ^.backgroundColor := (
                                  if(curAnswer.contains(index)) {
                                    "#FFFF00"
                                  } else {
                                    "transparent"
                                  }
                                ),
                                ^.onMouseMove --> touchWord(index),
                                ^.onMouseDown ==> (
                                  (e: ReactEvent) => if(curAnswer.contains(index)) {
                                    e.stopPropagation
                                    startErase >> touchWord(index)
                                  } else {
                                    startHighlight >> touchWord(index)
                                  }
                                ),
                                Text.normalizeToken(sentence(index))
                              ))
                          ).toVdomArray),
                        <.div(
                          (0 until s.qaGroups.size).toVdomArray(groupIndex =>
                            <.div(
                              <.p(
                                Styles.bolded,
                                Text.normalizeToken(sentence(prompt.keywords(groupIndex)))
                              ),
                              <.ul(
                                Styles.listlessList,
                                (0 until s.qaGroups(groupIndex).qas.size).toVdomArray(qaIndex =>
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
                            Styles.goodGreen.when(curPotentialBonus > 0),
                            Styles.bolded.when(curPotentialBonus > 0),
                            s"${math.round(100 * curPotentialBonus).toInt}c"
                          )
                        ),
                        <.p(
                          <.input(
                            ^.`type` := "text",
                            ^.name := FieldLabels.feedbackLabel,
                            ^.placeholder := "Feedback? (Optional)",
                            ^.margin := "1px",
                            ^.padding := "1px",
                            ^.width := "484px"
                          )
                        ),
                        <.input(
                          ^.`type` := "submit",
                          ^.disabled := !s.qaGroups.forall(getCompleteQAPairs(_).size > 0),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := "submit"),
                        instructions
                      )
                  }))
          }))
    }
  }

  val FullUI = ScalaComponent.builder[Unit]("Full UI")
    .initialState(State.empty)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(_.backend.updateResponse)
    .build

}
