package turksem.scisrl

import cats.implicits._

import turksem._
import turksem.util._

import spacro.tasks.TaskClient
import spacro.tasks.FieldLabels
import spacro.tasks.Service
import spacro.ui._

import nlpdata.util.Text
import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags
import nlpdata.util.HasTokens.ops._
import nlpdata.structure.Word

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle.function.{all => Optics}
import monocle.macros.Lenses

/**
  * Client for the SciSRL task.
  * Extends TaskClient, which provides a bunch of fields (like `prompt`, which is the input to this task instance)
  * that extract data relevant to this HIT from the HTML on the underlying page.
  */
class SciSRLClient[SID : Reader : Writer](instructions: VdomTag)(
  implicit promptReader: Reader[SciSRLPrompt[SID]], // need bc macro serializers fail for superclass constructor parameters
  responseWriter: Writer[SciSRLResponse] // same as above
) extends TaskClient[SciSRLPrompt[SID], SciSRLResponse, Service.UnitRequest] {

  val colorChoices = Vector(
    "rgb(46, 151, 222)", // blue
    "rgb(234, 75, 54)", // red
    "rgb(29, 206, 108)", // foresty
    "rgb(156, 85, 183)", // violet
    "rgb(0, 189, 155)", // teal
    "rgb(233, 126, 6)", // orange
    "rgb(242, 197, 0)" // bright goldenrod
      // "rgb(51, 73, 95)", // slate
      // there is also off-white and gray
  )

  def colorTag(index: Int): TagMod =
    ^.color := colorChoices((index + colorChoices.size) % colorChoices.size)

  // higher-order and utility React components all defined in turksem.util

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[SciSRLApiRequest[SID], SciSRLApiResponse]
  import WebsocketLoadableComponent._

  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int)] // keyword index AMONG KEYWORDS, qa index, answer word index
  import HighlightingComponent._

  import HighlightableSentenceComponent._

  import KeyboardControlComponent._

  // represents the answer to one of the kinds of questions. spans can be non-contiguous (it's easier to implement this way anyway)
  @Lenses case class AnswerSpan(
    indices: Set[Int],
    isInvalid: Boolean) {
    def toOptionalSpan = isInvalid.ifTrue(Option(indices))
  }
  object AnswerSpan {
    val empty = AnswerSpan(Set.empty[Int], false)
  }

  // stores all relevant state for a single QA group, which corresponds to a verb / "event".
  // synthesizes all of the relevant natural language strings here in one place.
  // holds the sentence as a field just out of convenience for computing those strings.
  @Lenses case class QAGroup(
    groupIndex: Int,
    posTaggedSentence: Vector[Word],
    verbIndex: Int,
    inflectedForms: InflectedForms,
    spans: List[AnswerSpan]) { // always length 4

    val sentence = posTaggedSentence.map(_.token)

    def objSpan  = spans(0)
    def subjSpan = spans(1)
    def locSpan  = spans(2)
    def timeSpan = spans(3)

    val propositionObject = Proposition(
      subj = subjSpan.toOptionalSpan,
      obj  =  objSpan.toOptionalSpan,
      loc  =  locSpan.toOptionalSpan,
      time = timeSpan.toOptionalSpan)

    val originalVerb = Text.normalizeToken(sentence(verbIndex)).lowerCase
    val verbStem = inflectedForms.stem
    val singularActive = inflectedForms.present
    val passiveVerb = inflectedForms.pastParticiple
    val presentParticipleVerb = inflectedForms.presentParticiple

    private[this] def getSpanTextOpt(span: AnswerSpan): Option[String] = {
      if(span.isInvalid) None
      else if(span.indices.isEmpty) Some("something")
      else {
        val verbatimText = Text.renderSpan(sentence, span.indices)
        val text = if(span.indices.contains(0)) {
          verbatimText.head.toLower + verbatimText.substring(1)
        } else verbatimText
        Some(text)
      }
    }

    def verbSpan(text: String) = <.span(colorTag(groupIndex), text)

    val objTextOpt  = getSpanTextOpt(objSpan)
    val subjTextOpt = getSpanTextOpt(subjSpan)
    val locTextOpt  = getSpanTextOpt(locSpan)
    val timeTextOpt = getSpanTextOpt(timeSpan)

    val isObjectSingular = !objSpan.indices
      .map(i => posTaggedSentence(i).pos)
      .exists(PosTags.pluralPosTags.contains)

    val isSubjectSingular =
      if(subjTextOpt.fold(false)(_ == "something")) true
      else originalVerb.equalsIgnoreCase(inflectedForms.present)

    val proposition = (subjTextOpt, objTextOpt) match {
      case (None, None) => <.span("something ", verbSpan(s"is $passiveVerb"))
      case (None, Some(obj)) =>
        val copula = if(isObjectSingular) "is" else "are"
        <.span(s"$obj ", verbSpan(s"$copula $passiveVerb"))
      case (Some(subj), objOpt) =>
        val verb = if(isSubjectSingular) singularActive else originalVerb
        objOpt match {
          case None => <.span(s"$subj ", verbSpan(verb))
          case Some(obj) => <.span(s"$subj ", verbSpan(verb), s" $obj")
        }
    }

    val propositionGerund = (subjTextOpt, objTextOpt) match {
      case (None, None) => <.span("something ", verbSpan(s"being $passiveVerb"))
      case (None, Some(obj)) => <.span(s"$obj ", verbSpan(s"being $passiveVerb"))
      case (Some(subj), None) => <.span(s"$subj ", verbSpan(presentParticipleVerb))
      case (Some(subj), Some(obj)) => <.span(s"$subj ", verbSpan(presentParticipleVerb), s" $obj")
    }

    // maybe we should do singular agreement since it's agreeing with "what", but I think it sounds better to adjust for plurals in the passive case
    val objQuestion = subjTextOpt.fold(
      if(isObjectSingular) <.span("What ", verbSpan(s"is $passiveVerb"), "?") else <.span("What ", verbSpan(s"are $passiveVerb"), "?"))(subj =>
      if(isSubjectSingular) <.span(s"What does $subj ", verbSpan(verbStem), "?") else <.span(s"What do $subj ", verbSpan(verbStem), "?")
    )
    val subjQuestion = objTextOpt.fold(<.span("What ", verbSpan(singularActive), "?"))(obj =>
      <.span("What ", verbSpan(singularActive), s" $obj?") // singular agreement with "what" sounds a bit more natural here
    )
    private[this] def advQuestion(prefix: String) = (subjTextOpt, objTextOpt) match {
      case (None, None) => <.span(s"$prefix is something ", verbSpan(passiveVerb), "?")
      case (None, Some(obj)) =>
        val copula = if(isObjectSingular) "is" else "are"
        <.span(s"$prefix $copula $obj ", verbSpan(passiveVerb), "?")
      case (Some(subj), objOpt) =>
        val aux = if(isSubjectSingular) "does" else "do"
        objOpt match {
          case None => <.span(s"$prefix $aux $subj ", verbSpan(verbStem), "?")
          case Some(obj )=> <.span(s"$prefix $aux $subj ", verbSpan(verbStem), s" $obj?")
        }
    }
    val locQuestion = advQuestion("Where")
    val timeQuestion = advQuestion("When")
  }

  // global state for the interface.
  @Lenses case class State(
    qaGroups: List[QAGroup],
    enablers: Set[(Int, Int)],
    preventers: Set[(Int, Int)],
    curFocus: (Int, Int)) {// (group index, question index)
    val response = SciSRLResponse(
      qaGroups.map(_.propositionObject),
      enablers,
      preventers)
  }

  object State {
    // for before we have the response from the server
    val empty: State = State(Nil, Set.empty[(Int, Int)], Set.empty[(Int, Int)], (0, 0))
    // ...and for after we receive it
    def initFromResponse(response: SciSRLApiResponse): State = response match {
      case SciSRLApiResponse(posTaggedSentence, verbInflectedForms) =>
        val qaGroups = prompt.verbIndices.zip(verbInflectedForms).zipWithIndex.map {
          case ((verbIndex, inflectedForms), groupIndex) =>
            QAGroup(groupIndex, posTaggedSentence, verbIndex, inflectedForms, List.fill(4)(AnswerSpan.empty))
        }.toList
        State(qaGroups, Set.empty[(Int, Int)], Set.empty[(Int, Int)], (0, 0))
    }
  }

  // lenses for easy access/modifying fields

  def groupLens(groupIndex: Int) = State.qaGroups
    .composeOptional(Optics.index(groupIndex))

  def answerLens(groupIndex: Int, questionIndex: Int) = groupLens(groupIndex)
    .composeLens(QAGroup.spans)
    .composeOptional(Optics.index(questionIndex))

  def enablerLens(enabler: Int, enablee: Int) = State.enablers
    .composeLens(Optics.at((enabler, enablee)))

  def preventerLens(preventer: Int, preventee: Int) = State.preventers
    .composeLens(Optics.at((preventer, preventee)))

  // used to determine whether the HIT can be submitted

  def answerIsComplete(answer: AnswerSpan): Boolean = answer.isInvalid || answer.indices.nonEmpty
  def groupIsComplete(group: QAGroup): Boolean = group.spans.forall(answerIsComplete)
  def allIsComplete(state: State): Boolean = state.qaGroups.forall(groupIsComplete)

  // the backend object defines all of the relevant callbacks and the render function; this is where the action is
  class FullUIBackend(scope: BackendScope[Unit, State]) {

    // TODO figure out how to do this with lenses, if possible; issue is need to carry indices down through nesting
    // sync up global state with the internal state of the higher-order HighlightingComponent
    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.modify(groups =>
          groups.zipWithIndex.map {
            case (group, gIndex) =>
              group.copy(
                spans = group.spans.zipWithIndex.map {
                  case (answerSpan, questionIndex) =>
                    answerSpan.copy(
                      indices = hs.span.collect {
                        case (`gIndex`, `questionIndex`, aIndex) => aIndex
                      }.toSet
                    )
                }
              )
          }
        )
      )

    def toggleInvalid(groupIndex: Int, questionIndex: Int): Callback =
      scope.modState(
        answerLens(groupIndex, questionIndex).composeLens(AnswerSpan.isInvalid).modify(!_)
      )

    def toggleCurrentInvalid: Callback = scope.state >>= (s =>
      toggleInvalid(s.curFocus._1, s.curFocus._2)
    )

    // all keyboard controls are handled here
    def handleKey(e: ReactKeyboardEvent): Callback = {
      def next = scope.modState((s: State) =>
        State.curFocus.modify {
          case (curGroup, curQuestion) => if(curQuestion == 3) ((curGroup + 1) % s.qaGroups.size, 0) else (curGroup, curQuestion + 1)
      }(s))
      def prev = scope.modState((s: State) =>
        State.curFocus.modify {
          case (curGroup, curQuestion) => if(curQuestion == 0) ((curGroup - 1 + s.qaGroups.size) % s.qaGroups.size, 3) else (curGroup, curQuestion - 1)
        }(s))
      if(isNotAssigned) {
        Callback.empty
      } else CallbackOption.keyCodeSwitch(e) {
        // I added a bunch of extra possible controls because why not.
        case KeyCode.Down => next
        case KeyCode.S => next
        case KeyCode.Up => prev
        case KeyCode.W => next
        case KeyCode.Left => toggleCurrentInvalid
        case KeyCode.A => toggleCurrentInvalid
        case KeyCode.Right => toggleCurrentInvalid
        case KeyCode.D => toggleCurrentInvalid
        case KeyCode.Space => toggleCurrentInvalid
      } >> e.preventDefaultCB
    }

    // should be called any time the relevant state changes what the response to the HIT should be.
    // you can see this is the case below: `.componentDidUpdate(context => context.$.backend.updateResponse)` does this.
    def updateResponse: Callback = scope.state.map { st =>
      // setResponse is taken from the TaskClient superclass and writes the response data to the DOM to be submitted through Turk.
      println("setting response... " + write(st.response))
      setResponse(st.response)
    }

    def qaField(state: State, groupIndex: Int, questionIndex: Int, sentence: Vector[String], question: VdomTag) = {
      val isFocused = state.curFocus == (groupIndex, questionIndex)
      val group = state.qaGroups(groupIndex)
      val answer = group.spans(questionIndex)
      <.div(
        ^.overflow := "hidden",
        <.div(
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "55px",
          (^.backgroundColor := "#E01010").when(answer.isInvalid),
          ^.onClick --> toggleInvalid(groupIndex, questionIndex),
          "Invalid"
        ),
        <.span(
          Styles.bolded.when(isFocused),
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          question
        ),
        <.div(
          Styles.answerIndicator,
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.width := "25px",
          "-->".when(isFocused)
        ),
        <.div(
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          (^.color := "#CCCCCC").when((answer.isInvalid || answer.indices.isEmpty)),
          if(answer.isInvalid) "N/A"
          else if(isFocused && answer.indices.isEmpty) "Highlight answer above, move with arrow keys"
          else Text.renderSpan(sentence, answer.indices)
        )
      )
    }

    def makeEventRelationsForm(state: State, groupIndex: Int) = {
      val group = state.qaGroups(groupIndex)
      val prop = group.propositionGerund
        <.div(
          <.p("Please check the propositions below that are enabled by ", prop, "."),
          <.ul(
            ^.classSet1("list-unstyled"),
            state.qaGroups.zipWithIndex.filter(_._2 != groupIndex).toVdomArray {
              case (relGroup, relGroupIndex) =>
                val isPreventer = preventerLens(groupIndex, relGroupIndex).get(state)
                <.li(
                  <.input(
                    ^.`type` := "checkbox",
                    ^.disabled := isPreventer,
                    ^.checked := enablerLens(groupIndex, relGroupIndex).get(state),
                    ^.onChange --> scope.modState(enablerLens(groupIndex, relGroupIndex).modify(!_))
                  ),
                  " ",
                  relGroup.proposition
                )
            }
          ),
          <.p("Please check the propositions below that are prevented by ", prop, "."),
          <.ul(
            ^.classSet1("list-unstyled"),
            state.qaGroups.zipWithIndex.filter(_._2 != groupIndex).toVdomArray {
              case (relGroup, relGroupIndex) =>
                val isEnabler = enablerLens(groupIndex, relGroupIndex).get(state)
                <.li(
                  <.input(
                    ^.`type` := "checkbox",
                    ^.disabled := isEnabler,
                    ^.checked := preventerLens(groupIndex, relGroupIndex).get(state),
                    ^.onChange --> scope.modState(preventerLens(groupIndex, relGroupIndex).modify(!_))
                  ),
                  " ",
                  relGroup.proposition
                )
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = SciSRLApiRequest(prompt),
          onLoad = ((response: SciSRLApiResponse) => scope.setState(State.initFromResponse(response))),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(SciSRLApiResponse(posTaggedSentence, _), _) =>
              val sentence = posTaggedSentence.map(_.token)
              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned, update = updateHighlights, render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(_, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val (curGroupIndex, curQuestionIndex) = s.curFocus

                      val curAnswer = spans.collect {
                        case (`curGroupIndex`, `curQuestionIndex`, ansIndex) => ansIndex
                      }

                      def touchWord(i: Int) = touchElement((curGroupIndex, curQuestionIndex, i))

                      <.div(
                        // to make room for fixed position sentence at top.
                        // TODO could set dynamically to accommodate long sentences
                        ^.marginTop := "50px",
                        ^.classSet1("container-fluid"),
                        // sentence and QA pairs
                        KeyboardControl(
                          KeyboardControlProps(
                            handleKey = handleKey,
                            message = (if(isNotAssigned) "Accept assignment to start" else "Click here for keyboard controls"),
                            render = <.div(
                              ^.onMouseUp --> stopHighlight,
                              ^.onMouseDown --> startHighlight,
                              Styles.mainContent,
                              HighlightableSentence(
                                HighlightableSentenceProps(
                                  sentence = sentence,
                                  styleForIndex = ((i: Int) =>
                                    prompt.verbIndices.indexOf(i).onlyIf(_ != -1).map(index =>
                                      TagMod(Styles.bolded, colorTag(index))
                                    ).whenDefined
                                  ),
                                  highlightedIndices = curAnswer,
                                  startHighlight = startHighlight,
                                  startErase = startErase,
                                  touchWord = touchWord,
                                  render = (elements =>
                                    <.blockquote(
                                      ^.position := "fixed",
                                      ^.top := "0px",
                                      ^.backgroundColor := "white",
                                      ^.classSet1("blockquote"),
                                      Styles.unselectable,
                                      elements.toVdomArray)
                                  ))
                              ),
                              <.div(
                                s.qaGroups.zipWithIndex.toVdomArray {
                                  case (group, groupIndex) =>
                                    <.div(
                                      <.h4(
                                        Styles.bolded,
                                        Text.normalizeToken(sentence(prompt.verbIndices(groupIndex)))
                                      ),
                                      qaField(s, groupIndex, 0, sentence, group.objQuestion),
                                      qaField(s, groupIndex, 1, sentence, group.subjQuestion),
                                      qaField(s, groupIndex, 2, sentence, group.locQuestion),
                                      qaField(s, groupIndex, 3, sentence, group.timeQuestion))
                                }
                              )
                            )
                          )
                        ),
                        // relations between predicates
                        <.div(s.qaGroups.indices.toVdomArray(makeEventRelationsForm(s, _))),
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
                          ^.disabled := !allIsComplete(s),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := "submit"),
                        instructions
                      )
                  }))
          }))
    }
  }

  val FullUI = ScalaComponent.builder[Unit]("Full UI")
    .initialState(State.empty).renderBackend[FullUIBackend]
    .componentDidUpdate(_.backend.updateResponse)
    .build

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

}
