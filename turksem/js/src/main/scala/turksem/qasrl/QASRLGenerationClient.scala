package turksem.qasrl

import scala.collection.mutable

import cats.data.NonEmptyList
import cats.implicits._

import turksem._

import turksem.qamr._
import turksem.util._
import turkey.tasks._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
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

  import HighlightableSentenceComponent._

  def emptyQA(keyword: Int) = WordedQAPair(keyword, "", Set.empty[Int])

  // TODO store QAState for efficiency so we only need to template-analyze one question at once

  // sealed trait QAState
  // case object Complete extends QAState
  // case object InProgress extends QAState
  // case class Invalid(numGoodCharacters: Int) extends QAState

  @Lenses case class QAGroup(
    verbIndex: Int,
    template: QASRLTemplate,
    qas: List[WordedQAPair],
    inputRefOpts: List[Option[html.Element]]) {
    def withNewEmptyQA: QAGroup = copy(qas = this.qas :+ (emptyQA(verbIndex)), inputRefOpts = this.inputRefOpts :+ None)
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
            QAGroup(verbIndex, template, Nil, Nil)
        }
        State(qaGroups, None)
    }
  }

  def qaLens(groupIndex: Int, qaIndex: Int) = State.qaGroups
    .composeOptional(index(groupIndex))
    .composeLens(QAGroup.qas)
    .composeOptional(index(qaIndex))

  def questionLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
    // .composeLens(first)
    .composeLens(WordedQAPair.question)

  // def qaStateLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
  //   .composeLens(second)

  // def getQAState(template: QASRLTemplate, wqa: WordedQAPair): QAState =
  //   template.processStringFully(wqa) match {
  //     case Left(AggregatedInvalidState(_, numGoodCharacters)) => Invalid(numGoodCharacters)
  //     case Right(validStates) => if(validStates.exists(_.isComplete)) Complete else InProgress
  //   }

  def isComplete(
    template: QASRLTemplate,
    wqa: WordedQAPair
  ): Boolean = {
    !wqa.question.isEmpty && !wqa.answer.isEmpty && template.isValid(wqa.question)
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

    val allInputRefs = mutable.Map.empty[(Int, Int), html.Element]

    var isBlurEnabled: Boolean = false

    def setBlurEnabled(b: Boolean) = Callback(isBlurEnabled = b)

    def setInputRef(groupIndex: Int, qaIndex: Int): html.Element => Unit =
      (element: html.Element) => allInputRefs.put((groupIndex, qaIndex), element)

    def updateInputRefs: Callback = scope.modState(
      State.qaGroups.composeTraversal(eachIndexed).modify {
        case (group, groupIndex) => QAGroup.inputRefOpts.composeTraversal(eachIndexed).modify {
          case (_, qaIndex) => allInputRefs.get((groupIndex, qaIndex)) -> qaIndex
        }(group) -> groupIndex
      }
    )

    val canvas = scala.scalajs.js.Dynamic.global.document.createElement("canvas")
    val context = canvas.getContext("2d")

    def addQAFields: (State => State) = State.qaGroups.modify(groups =>
      groups.map { group =>
        if(group.qas.forall(isComplete(group.template, _))) group.withNewEmptyQA
        else group
      }
    )

    def moveToNextQuestion: Callback = scope.state.flatMap { s =>
      s.curFocus.fold(Callback.empty) { focus =>
        val allFocusablePairs = s.qaGroups.toVector.zipWithIndex.flatMap {
          case (group, groupIndex) => group.qas.zipWithIndex.map {
            case (_, qaIndex) => (groupIndex, qaIndex)
          }
        }
        val curIndex = allFocusablePairs.indexOf(focus)
        val newIndex = (curIndex + 1) % allFocusablePairs.size

        Callback(allInputRefs(allFocusablePairs(newIndex)).focus)
        // s.copy(curFocus = Some(allFocusablePairs(newIndex)))
      }
    }

    // def resolveQACompletionState(groupIndex: Int, qaIndex: Int, qaState: QAState): Callback =
    //   scope.modState(qaStateLens(groupIndex, qaIndex).set(qaState))

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.composeTraversal(eachIndexed).modify {
          case (group, groupIndex) => QAGroup.qas.composeTraversal(eachIndexed).modify {
            case (wqa, qaIndex) => WordedQAPair.answer.set(
              hs.span.collect {
                case (`groupIndex`, `qaIndex`, aIndex) => aIndex
              }.toSet)(wqa) -> qaIndex
          }(group) -> groupIndex
        } andThen addQAFields
      )

    def updateResponse: Callback = scope.state.map { st =>
      setResponse(getAllCompleteQAPairs(st.qaGroups))
    }

    def qaField(
      s: State,
      sentence: Vector[String],
      groupIndex: Int,
      qaIndex: Int
    ) = s match {
      case State(qaGroups, curFocus) =>
        val qaGroup = qaGroups(groupIndex)
        val template = qaGroup.template
        val isFocused = curFocus == Some(groupIndex, qaIndex)
        val numQAsInGroup = qaGroup.qas.size
        val WordedQAPair(_, question, answer) = qaGroup.qas(qaIndex)
        val isAnswerEmpty = answer.isEmpty
        val nextBonus = bonusFor(getAllCompleteQAPairs(qaGroups).size + 1)

        case class Suggestion(fullText: String, isComplete: Boolean)

        case class AutocompleteState(suggestions: NonEmptyList[Suggestion], badStartIndexOpt: Option[Int])

        // NOTE this is empty iff question is complete/valid
        val autocompleteStateOpt = {
          import QASRLTemplate._
          def createSuggestion(ips: InProgressState): Suggestion =
            Suggestion(ips.fullText, template.isAlmostComplete(ips))

          // technically maybe should collapse together by full text in case there are two (one complete, one not)
          def makeList(goodStates: NonEmptyList[ValidState]) = NonEmptyList.fromList(
            goodStates.toList.collect { case ips @ InProgressState(_, _, _) => createSuggestion(ips) }
              .toSet.toList
              .sortBy((vs: Suggestion) => vs.fullText)
          )

          template.processStringFully(question) match {
            case Left(AggregatedInvalidState(lastGoodStates, badStartIndex)) =>
              makeList(lastGoodStates).map(options => AutocompleteState(options, Some(badStartIndex)))
            case Right(goodStates) =>
              makeList(goodStates).map(options => AutocompleteState(options, None))
          }
        }

        val isFieldComplete = autocompleteStateOpt.isEmpty
        val isFieldInProgress = !isFieldComplete && question.nonEmpty

        <.div(
          ^.overflow := "hidden",
          IntState.LocalState(
            IntState.LocalStateProps(
              initialValue = 0,
              render = (highlightedIndex: Int, setHighlightedIndex: Int => Callback) => {
                def selectItem(suggestion: Suggestion): Callback = suggestion match {
                  case Suggestion(string, _) =>
                    scope.modState(questionLens(groupIndex, qaIndex).set(string)) >>
                      (if(isFieldComplete) setBlurEnabled(true) >> moveToNextQuestion else Callback.empty)
                }
                def handleKey(autocompleteStateOpt: Option[AutocompleteState])(e: ReactKeyboardEvent): Callback = {
                  val genHandlers = CallbackOption.keyCodeSwitch(e) {
                    case KeyCode.Tab => setBlurEnabled(true)
                    case KeyCode.Escape => setBlurEnabled(true)
                  } orElse CallbackOption.keyCodeSwitch(e, shiftKey = true) {
                    case KeyCode.Tab => setBlurEnabled(true)
                  }
                  val menuHandlers = autocompleteStateOpt.fold(
                    CallbackOption.keyCodeSwitch(e) {
                      case KeyCode.Enter => setBlurEnabled(true) >> moveToNextQuestion >> e.preventDefaultCB
                    } orElse genHandlers
                  ) { acs =>
                    val menuItems = acs.suggestions
                    def next = setHighlightedIndex((highlightedIndex + 1) % menuItems.size.toInt)
                    def prev = setHighlightedIndex((highlightedIndex - 1 + menuItems.size.toInt) % menuItems.size.toInt)
                    CallbackOption.keyCodeSwitch(e) {
                      case KeyCode.Down => next >> e.preventDefaultCB
                      case KeyCode.Up => prev >> e.preventDefaultCB
                      case KeyCode.Enter => selectItem(menuItems.toList(highlightedIndex)) >> e.preventDefaultCB
                    } orElse genHandlers
                  }

                  genHandlers orElse menuHandlers
                }
                val bgStyleOpt = if(isFieldComplete) {
                  Some(^.backgroundColor := "rgba(0, 255, 0, 0.3)")
                } else if(isFieldInProgress && !isFocused) {
                  Some(^.backgroundColor := "rgba(255, 255, 0, 0.3)")
                } else None
                <.div(
                  Reference(
                    ReferenceProps(
                      referencedTag = <.input(
                        (^.disabled := true).when(isNotAssigned),
                        bgStyleOpt.whenDefined,
                        ^.float := "left",
                        ^.`type` := "text",
                        ^.placeholder := (
                          if(qaIndex == 0) "Question (required)"
                          else s"Question (+${math.round(100 * nextBonus).toInt}c)"
                        ),
                        ^.margin := s"1px",
                        ^.padding := s"1px",
                        ^.width := "480px",
                        ^.onKeyDown ==> handleKey(autocompleteStateOpt),
                        ^.onChange ==> (
                          (e: ReactEventFromInput) => {
                            val newValue = e.target.value
                            scope.modState(questionLens(groupIndex, qaIndex).set(newValue) andThen addQAFields)
                          }),
                        ^.onFocus --> scope.modState(State.curFocus.set(Some((groupIndex, qaIndex)))),
                        ^.onBlur --> (if(isBlurEnabled) scope.modState(State.curFocus.set(None)) else Callback(allInputRefs((groupIndex, qaIndex)).focus)),
                        ^.value := question
                      ),
                      render = {
                        case (input, refOpt) =>
                          <.div(
                            input.ref(setInputRef(groupIndex, qaIndex)),
                            (for {
                               AutocompleteState(suggestions, badStartIndexOpt) <- autocompleteStateOpt
                               ref <- allInputRefs.get((groupIndex, qaIndex)) // refOpt
                             } yield {
                               val rect = ref.getBoundingClientRect
                               val left = dom.window.pageXOffset + math.round(rect.left)
                               val bottom = dom.window.pageYOffset + math.round(rect.bottom)
                               val width = math.round(rect.width)

                               def itemBgStyle(tokenIndex: Int, isCompletion: Boolean) =
                                 if(tokenIndex == highlightedIndex && isCompletion) Some(^.backgroundColor := "rgba(0, 255, 0, 0.7)")
                                 else if(tokenIndex == highlightedIndex) Some(^.backgroundColor := "rgb(40, 162, 254)")
                                 else if(isCompletion) Some(^.backgroundColor := "rgba(0, 255, 0, 0.3)")
                                 else None

                               <.div(
                                 ^.position := "absolute",
                                 ^.top := s"${bottom}px",
                                 ^.left := s"${left}px",
                                 ^.minWidth := s"${width}px",
                                 ^.border := "solid 1px #ccc",
                                 ^.borderRadius := "3px",
                                 ^.boxShadow := "0 2px 12px rgba(0, 0, 0, 0.1)",
                                 ^.background := "rgba(255, 255, 255, 0.9)",
                                 ^.padding := "2px 0",
                                 ^.fontSize := "90%",
                                 ^.overflow := "auto",
                                 ^.maxHeight := "50%",
                                 suggestions.zipWithIndex.toList.toVdomArray {
                                   case (ts @ Suggestion(text, isCompletion), tokenIndex) =>
                                     <.div(
                                       itemBgStyle(tokenIndex, isCompletion).whenDefined,
                                       ^.padding := "2px 6px",
                                       ^.cursor := "default",
                                       ^.onMouseEnter --> setHighlightedIndex(tokenIndex),
                                       ^.onClick --> selectItem(ts),
                                       badStartIndexOpt match {
                                         case None => text
                                         case Some(badStartIndex) =>
                                           <.span(
                                             <.span(text.substring(0, badStartIndex)),
                                             <.span(
                                               ^.backgroundColor := "rgba(255, 0, 0, 0.3)",
                                               text.substring(badStartIndex)
                                             )
                                           )
                                       }
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
                        ^.classSet1("container-fluid"),
                        ^.onMouseEnter --> setBlurEnabled(false), // TODO might not need..?
                        ^.onMouseMove --> setBlurEnabled(false),
                        ^.onMouseLeave --> setBlurEnabled(true),
                        ^.onMouseUp --> stopHighlight,
                        ^.onMouseDown --> startHighlight,
                        Styles.mainContent,
                        <.p(<.span(Styles.badRed, """ Please read the detailed instructions at the bottom before you begin, """),
                            """ so you can maximize your bonuses and avoid losing your qualification. """,
                            """ To begin working on this HIT, please request the question-answer writing accuracy qualification.
                                It is auto-granted. Also, while there may be few HITs available at any one time,
                                more will be continuously uploaded as they are completed. """),
                        <.hr(),
                        HighlightableSentence(
                          HighlightableSentenceProps(
                            sentence = sentence,
                            specialWordIndices = prompt.keywords.toSet,
                            highlightedIndices = curAnswer,
                            startHighlight = startHighlight,
                            startErase = startErase,
                            touchWord = touchWord,
                            render = (elements =>
                              <.blockquote(
                                ^.classSet1("blockquote"),
                                Styles.unselectable,
                                elements.toVdomArray)
                            ))
                        ),
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
    .componentDidUpdate(context => context.backend.updateResponse)
    .build

}
