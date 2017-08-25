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
  instructions: VdomTag)(
  implicit promptReader: Reader[GenerationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[WordedQAPair]] // same as above
) extends TaskClient[GenerationPrompt[SID], List[WordedQAPair]] {

  import QASRLSettings._

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

  sealed trait QAState
  case object Complete extends QAState
  case object InProgress extends QAState
  case object Invalid extends QAState

  @Lenses case class QAPair(
    question: String,
    answer: Set[Int],
    state: QAState)
  object QAPair {
    val empty = QAPair("", Set.empty[Int], InProgress)
  }

  @Lenses case class QAGroup(
    verbIndex: Int,
    template: QASRLTemplate,
    qas: List[QAPair]) {
    def withNewEmptyQA: QAGroup = copy(qas = this.qas :+ (QAPair.empty))
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
          case IndexWithInflectedForms(verbIndex, forms) =>
            val slots = new Slots(sentence, forms)
            val template = new QASRLTemplate(slots)
            QAGroup(verbIndex, template, List(QAPair.empty))
        }
        State(qaGroups, None)
    }
  }

  def qaLens(groupIndex: Int, qaIndex: Int) = State.qaGroups
    .composeOptional(index(groupIndex))
    .composeLens(QAGroup.qas)
    .composeOptional(index(qaIndex))

  def questionLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
    .composeLens(QAPair.question)

  def qaStateLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
    .composeLens(QAPair.state)

  def getQAState(template: QASRLTemplate, qa: QAPair): QAState =
    template.processStringFully(qa.question) match {
      case Left(QASRLTemplate.AggregatedInvalidState(_, numGoodCharacters)) => Invalid
      case Right(validStates) => if(validStates.exists(_.isComplete)) Complete else InProgress
    }

  def isComplete(qa: QAPair): Boolean = qa.state == Complete && qa.answer.nonEmpty

  def getCompleteQAPairs(group: QAGroup): List[WordedQAPair] = for {
    qa <- group.qas
    if isComplete(qa)
  } yield WordedQAPair(group.verbIndex, qa.question, qa.answer)

  def getAllCompleteQAPairs(groups: List[QAGroup]): List[WordedQAPair] =
    groups.flatMap(getCompleteQAPairs)

  // better: a PTraversal that doesn't ask for the index back. would fix the issue of the iso being bad
  def indexingIso[A] = Iso[List[A], List[(A, Int)]](_.zipWithIndex)(_.map(_._1))
  def eachIndexed[A] = indexingIso[A].composeTraversal(each)

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    // mutable backend stuff
    val allInputRefs = mutable.Map.empty[(Int, Int), html.Element]
    var isBlurEnabled: Boolean = true

    def setBlurEnabled(b: Boolean) = Callback(isBlurEnabled = b)

    def setInputRef(groupIndex: Int, qaIndex: Int): html.Element => Unit =
      (element: html.Element) => allInputRefs.put((groupIndex, qaIndex), element)

    def updateQAState(groupIndex: Int, qaIndex: Int)(s: State): State = {
      val group = s.qaGroups(groupIndex)
      val template = group.template
      val question = group.qas(qaIndex).question

      template.processStringFully(question) match {
        case Left(QASRLTemplate.AggregatedInvalidState(_, _)) =>
          qaStateLens(groupIndex, qaIndex).set(Invalid)(s)
        case Right(goodStates) =>
          if(goodStates.exists(_.isComplete)) qaStateLens(groupIndex, qaIndex).set(Complete)(s)
          else qaStateLens(groupIndex, qaIndex).set(InProgress)(s)
      }
    }

    def addQAFields: (State => State) = State.qaGroups.modify(groups =>
      groups.map { group =>
        if(group.qas.forall(isComplete)) group.withNewEmptyQA
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
      }
    }

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.composeTraversal(eachIndexed).modify {
          case (group, groupIndex) => QAGroup.qas.composeTraversal(eachIndexed).modify {
            case (qa, qaIndex) => QAPair.answer.set(
              hs.span.collect {
                case (`groupIndex`, `qaIndex`, aIndex) => aIndex
              }.toSet)(qa) -> qaIndex
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
        val QAPair(question, answer, qaState) = qaGroup.qas(qaIndex)

        val isAnswerEmpty = answer.isEmpty
        val nextBonus = bonusPerQuestion

        case class Suggestion(fullText: String, isComplete: Boolean)

        case class AutocompleteState(suggestions: NonEmptyList[Suggestion], badStartIndexOpt: Option[Int])

        // NOTE this is empty iff question is complete/valid
        val autocompleteStateOpt = if(!isFocused) None else {
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

        def handleQuestionChange(newQuestion: String) = {
          scope.modState(questionLens(groupIndex, qaIndex).set(newQuestion) andThen updateQAState(groupIndex, qaIndex) andThen addQAFields)
        }

        <.div(
          ^.overflow := "hidden",
          IntState.LocalState(
            IntState.LocalStateProps(
              initialValue = 0,
              render = (highlightedIndex: Int, setHighlightedIndex: Int => Callback) => {

                def selectItem(suggestion: Suggestion): Callback = handleQuestionChange(suggestion.fullText)

                def handleKey(e: ReactKeyboardEvent): Callback = {
                  val genHandlers = CallbackOption.keyCodeSwitch(e) {
                    case KeyCode.Tab => setBlurEnabled(true)
                    case KeyCode.Escape => setBlurEnabled(true)
                  } orElse CallbackOption.keyCodeSwitch(e, shiftKey = true) {
                    case KeyCode.Tab => setBlurEnabled(true)
                  }
                  val menuHandlers =
                    if(qaState == Complete) {
                      CallbackOption.keyCodeSwitch(e) {
                        case KeyCode.Enter => setBlurEnabled(true) >> moveToNextQuestion >> e.preventDefaultCB
                      }
                    } else {
                      autocompleteStateOpt.fold(CallbackOption.pass) { acs =>
                        val menuItems = acs.suggestions
                        def next = setHighlightedIndex((highlightedIndex + 1) % menuItems.size.toInt)
                        def prev = setHighlightedIndex((highlightedIndex - 1 + menuItems.size.toInt) % menuItems.size.toInt)
                        CallbackOption.keyCodeSwitch(e) {
                          case KeyCode.Down => next >> e.preventDefaultCB
                          case KeyCode.Up => prev >> e.preventDefaultCB
                          case KeyCode.Enter => selectItem(menuItems.toList(highlightedIndex)) >> e.preventDefaultCB
                        }
                      }
                  }

                  genHandlers orElse menuHandlers
                }

                val bgStyle = qaState match {
                  case Complete => ^.backgroundColor := "rgba(0, 255, 0, 0.3)"
                  case InProgress if !isFocused && question.nonEmpty => ^.backgroundColor := "rgba(255, 255, 0, 0.3)"
                  case Invalid if !isFocused => ^.backgroundColor := "rgba(255, 0, 0, 0.3)"
                  case _ => EmptyVdom
                }

                <.div(
                  Reference(
                    ReferenceProps(
                      referencedTag = <.input(
                        (^.disabled := true).when(isNotAssigned),
                        bgStyle,
                        ^.float := "left",
                        ^.`type` := "text",
                        ^.placeholder := (
                          if(qaIndex == 0) "Question (required)"
                          else s"Question (+${math.round(100 * nextBonus).toInt}c)"
                        ),
                        ^.margin := s"1px",
                        ^.padding := s"1px",
                        ^.width := "480px",
                        ^.onKeyDown ==> handleKey,
                        ^.onChange ==> ((e: ReactEventFromInput) => handleQuestionChange(e.target.value)),
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
                                 ^.zIndex := "10",
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
          request = QASRLGenerationApiRequest(prompt),
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

                      val curPotentialBonus = generationBonus(s.qaGroups.size, curCompleteQAPairs.size)

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
                            styleForIndex = ((i: Int) =>
                              prompt.keywords.indexOf(i).onlyIf(_ != -1).map(index =>
                                TagMod(Styles.specialWord, Styles.niceBlue)
                              ).whenDefined
                            ),
                            highlightedIndices = curAnswer,
                            startHighlight = startHighlight,
                            startErase = startErase,
                            touchWord = touchWord,
                            render = (elements =>
                              <.blockquote(
                                ^.onMouseEnter --> setBlurEnabled(false), // TODO might not need..?
                                ^.onMouseMove --> setBlurEnabled(false),
                                ^.onMouseLeave --> setBlurEnabled(true),
                                ^.classSet1("blockquote"),
                                Styles.unselectable,
                                elements.toVdomArray)
                            ))
                        ),
                        <.div(
                          (0 until s.qaGroups.size).toVdomArray(groupIndex =>
                            <.div(
                              <.h6(
                                ^.marginTop := "15px",
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
                          ^.marginTop := "20px",
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
                          ^.classSet1("btn btn-primary"),
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
