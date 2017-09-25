package turksem.qasrl

import scala.collection.mutable

import cats.Order
import cats.Monoid
import cats.Applicative
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
import monocle.function.{all => Optics}
import monocle.std.{list => ListOptics}
import monocle.std.{option => OptionOptics}
import monocle.syntax._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.CatsReact._

class QASRLGenerationClient[SID : Reader : Writer](
  instructions: VdomTag)(
  implicit promptReader: Reader[GenerationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[VerbQA]] // same as above
) extends TaskClient[GenerationPrompt[SID], List[VerbQA]] {

  // for monoid on Callback
  implicit def appMonoid[F[_]: Applicative, A: Monoid] = Applicative.monoid[F, A]

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[QASRLGenerationApiRequest[SID], QASRLGenerationApiResponse]
  import WebsocketLoadableComponent._
  val SpanHighlightingComponent = new SpanHighlightingComponent[(Int, Int)] // keyword index AMONG KEYWORDS, qa index
  import SpanHighlightingComponent._

  val IntState = new LocalStateComponent[Int]

  import MultiContigSpanHighlightableSentenceComponent._

  sealed trait QAState {
    final def isComplete: Boolean = this match {
      case Complete(_) => true
      case _ => false
    }
  }
  case class Complete(frames: Set[(Frame, ArgumentSlot)]) extends QAState
  case object InProgress extends QAState
  case object Invalid extends QAState
  case object Redundant extends QAState

  @Lenses case class QAPair(
    question: String,
    answers: List[Set[Int]],
    state: QAState)
  object QAPair {
    val empty = QAPair("", List.empty[Set[Int]], InProgress)
  }

  @Lenses case class QAGroup(
    verbIndex: Int,
    template: QASRLStatefulTemplate,
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
      case QASRLGenerationApiResponse(_, sentence, indicesWithTemplates) =>
        val qaGroups = indicesWithTemplates.map {
          case IndexWithInflectedForms(verbIndex, forms) =>
            val slots = new TemplateStateMachine(sentence, forms)
            val template = new QASRLStatefulTemplate(slots)
            QAGroup(verbIndex, template, List(QAPair.empty))
        }
        State(qaGroups, None)
    }
  }

  def qaLens(groupIndex: Int, qaIndex: Int) = State.qaGroups
    .composeOptional(Optics.index(groupIndex))
    .composeLens(QAGroup.qas)
    .composeOptional(Optics.index(qaIndex))

  def questionLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
    .composeLens(QAPair.question)

  def qaStateLens(groupIndex: Int, qaIndex: Int) = qaLens(groupIndex, qaIndex)
    .composeLens(QAPair.state)

  def isQuestionComplete(qa: QAPair): Boolean = qa.state.isComplete

  def isComplete(qa: QAPair): Boolean = qa.state.isComplete && qa.answers.nonEmpty

  def getCompleteQAPairs(group: QAGroup): List[VerbQA] = for {
    qa <- group.qas
    if isComplete(qa)
  } yield VerbQA(group.verbIndex, qa.question, qa.answers)

  def getAllCompleteQAPairs(groups: List[QAGroup]): List[VerbQA] =
    groups.flatMap(getCompleteQAPairs)

  // better: a PTraversal that doesn't ask for the index back. would fix the issue of the iso being bad
  def indexingIso[A] = Iso[List[A], List[(A, Int)]](_.zipWithIndex)(_.map(_._1))
  def eachIndexed[A] = indexingIso[A].composeTraversal(Optics.each)

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
        case Left(QASRLStatefulTemplate.AggregatedInvalidState(_, _)) =>
          qaStateLens(groupIndex, qaIndex).set(Invalid)(s)
        case Right(goodStates) =>
          if(goodStates.exists(_.isComplete)) {
            if(group.qas.map(_.question).indexOf(question) < qaIndex) {
              qaStateLens(groupIndex, qaIndex).set(Redundant)(s)
            } else {
              val frames = goodStates.toList.collect {
                case QASRLStatefulTemplate.Complete(
                  _, TemplateStateMachine.FrameState(Some(whWord), prepOpt, answerSlotOpt, frame)
                ) if (prepOpt.nonEmpty == frame.args.get(Obj2).nonEmptyAnd(_.isPrep)) &&
                    (!Set("who", "what").map(_.lowerCase).contains(whWord) || answerSlotOpt.nonEmpty) =>
                  (frame, answerSlotOpt.getOrElse(Adv(whWord)))
              }.toSet
              qaStateLens(groupIndex, qaIndex).set(Complete(frames))(s)
            }
          }
          else qaStateLens(groupIndex, qaIndex).set(InProgress)(s)
      }
    }

    def addQAFields: (State => State) = State.qaGroups.modify(groups =>
      groups.map { group =>
        if(group.qas.forall(isQuestionComplete)) group.withNewEmptyQA
        else group
      }
    )

    def moveToNextQuestion: Callback = scope.state.flatMap { s =>
      s.curFocus.foldMap { focus =>
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

    def updateHighlights(hs: SpanHighlightingState) =
      scope.modState(
        State.qaGroups.composeTraversal(eachIndexed).modify {
          case (group, groupIndex) => QAGroup.qas.composeTraversal(eachIndexed).modify {
            case (qa, qaIndex) => QAPair.answers.set(
              hs.spans((groupIndex, qaIndex)).map(_.indices)
            )(qa) -> qaIndex
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
      qaIndex: Int,
      nextPotentialBonus: Double
    ) = s match {
      case State(qaGroups, curFocus) =>
        val qaGroup = qaGroups(groupIndex)
        val template = qaGroup.template
        val isFocused = curFocus.nonEmptyAnd(_ == (groupIndex, qaIndex))
        val numQAsInGroup = qaGroup.qas.size
        val QAPair(question, answers, qaState) = qaGroup.qas(qaIndex)

        case class Suggestion(fullText: String, isComplete: Boolean)

        case class AutocompleteState(suggestions: NonEmptyList[Suggestion], badStartIndexOpt: Option[Int])

        // NOTE this is empty iff question is complete/valid
        val autocompleteStateOpt = if(!isFocused) None else {
          import turksem.qasrl.{QASRLStatefulTemplate => Template}
          def createSuggestion(ips: Template.InProgressState): Suggestion =
            Suggestion(ips.fullText, template.isAlmostComplete(ips))

          // technically maybe should collapse together by full text in case there are two (one complete, one not)
          // but that should never happen anyway
          def makeList(goodStates: NonEmptyList[Template.ValidState]) = NonEmptyList.fromList(
            goodStates.toList.collect { case ips @ Template.InProgressState(_, _, _, _) => createSuggestion(ips) }
              .toSet.toList
              .sortBy((vs: Suggestion) => vs.fullText)
          )

          val allLowercaseQuestionStrings = s.qaGroups.flatMap(_.qas).map(_.question.lowerCase).toSet

          template.processStringFully(question) match {
            case Left(Template.AggregatedInvalidState(lastGoodStates, badStartIndex)) =>
              makeList(lastGoodStates).map(options => AutocompleteState(options, Some(badStartIndex)))
            case Right(goodStates) =>
              val framesWithAnswerSlots = s.qaGroups.flatMap(_.qas).map(_.state).collect {
                case Complete(framesWithSlots) => framesWithSlots
              }.flatten
              val frameCounts = counts(framesWithAnswerSlots.map(_._1))
              val frameToFilledAnswerSlots = framesWithAnswerSlots.foldLeft(Map.empty[Frame, Set[ArgumentSlot]].withDefaultValue(Set.empty[ArgumentSlot])) {
                case (acc, (frame, slot)) => acc.updated(frame, acc(frame) + slot)
              }
              val framesByCountDecreasing = frameCounts.keys.toList.sortBy(f => -10 * frameCounts(f) + math.abs(f.args.size - 2))
              val allQuestions = framesByCountDecreasing.flatMap { frame =>
                val unAnsweredSlots = frame.args.keys.toSet -- frameToFilledAnswerSlots(frame)
                val coreArgQuestions = unAnsweredSlots.toList.flatMap(frame.questionsForSlot)
                val advQuestions = ArgumentSlot.allAdvSlots
                  .filterNot(frameToFilledAnswerSlots(frame).contains)
                  .flatMap(frame.questionsForSlot)
                coreArgQuestions ++ advQuestions
              }.filter(_.toLowerCase.startsWith(question.toLowerCase)).filterNot(q => allLowercaseQuestionStrings.contains(q.lowerCase)).distinct
              val questionSuggestions = allQuestions.flatMap(q =>
                template.processStringFully(q) match {
                  case Right(goodStates) if goodStates.exists(_.isComplete) => Some(Suggestion(q, true))
                  case _ => None
                }
              ).take(4) // number of suggested questions capped at 4 to filter out crowd of bad ones
              makeList(goodStates).map { options =>
                AutocompleteState(NonEmptyList.fromList(questionSuggestions).fold(options)(sugg => (sugg ++ options.toList).distinct(Order.by[Suggestion, String](_.fullText))), None)
              }
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
                    if(qaState.isComplete) {
                      CallbackOption.keyCodeSwitch(e) {
                        case KeyCode.Enter => setBlurEnabled(true) >> moveToNextQuestion >> e.preventDefaultCB
                      }
                    } else {
                      autocompleteStateOpt.foldMap { acs =>
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
                  case Complete(_) if answers.nonEmpty => ^.backgroundColor := "rgba(0, 255, 0, 0.3)"
                  case Redundant => ^.backgroundColor := "rgba(255, 255, 0, 0.3)"
                  case Invalid if !isFocused => ^.backgroundColor := "rgba(255, 0, 0, 0.3)"
                  case _ => EmptyVdom
                }

                <.div(
                  <.input(
                    (^.disabled := true).when(isNotAssigned),
                    bgStyle,
                    ^.float := "left",
                    ^.`type` := "text",
                    ^.placeholder := (
                      if(qaIndex == 0) ("Question about \"" + Text.normalizeToken(sentence(qaGroup.verbIndex)) + "\" (required)")
                      else ("Question about \"" + Text.normalizeToken(sentence(qaGroup.verbIndex)) + "\"" + s" (+${math.round(100 * nextPotentialBonus).toInt}c)")
                    ),
                    ^.padding := s"1px",
                    ^.width := "480px",
                    ^.onKeyDown ==> handleKey,
                    ^.onChange ==> ((e: ReactEventFromInput) => handleQuestionChange(e.target.value)),
                    ^.onFocus --> scope.modState(State.curFocus.set(Some((groupIndex, qaIndex)))),
                    ^.onBlur --> (
                      if(isBlurEnabled) scope.modState(State.curFocus.set(None))
                      else Callback(allInputRefs((groupIndex, qaIndex)).focus)
                    ),
                    ^.value := question
                  ).ref(setInputRef(groupIndex, qaIndex)),
                  (for {
                     AutocompleteState(suggestions, badStartIndexOpt) <- autocompleteStateOpt
                     ref <- allInputRefs.get((groupIndex, qaIndex))
                   } yield {
                     val rect = ref.getBoundingClientRect
                     val width = math.round(rect.width)
                     val bottom = math.round(ref.offsetTop + rect.height)
                     val left = math.round(ref.offsetLeft)

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
                       ^.background := "rgba(255, 255, 255, 1.0)",
                       ^.padding := "2px 0",
                       ^.fontSize := "90%",
                       ^.overflow := "auto",
                       ^.maxHeight := "600px",
                       ^.onMouseMove --> setBlurEnabled(false),
                       ^.onMouseLeave --> setBlurEnabled(true),
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
              })),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.onMouseMove --> setBlurEnabled(false),
            ^.onMouseLeave --> setBlurEnabled(true),
            {
              val answersString = (0 until answers.size).reverse
                .map(i => Text.renderSpan(sentence, answers(i)))
                .mkString(" / ")
              if(isFocused) {
                if(answers.isEmpty) <.span(
                  ^.color := "#CCCCCC",
                  "Highlight your answer above"
                ) else <.span(
                  answersString,
                  <.span(
                    (^.color := "#CCCCCC"),
                    " / Highlight another phrase to add an answer"
                  )
                )
              } else <.span(
                ^.onClick --> (Callback(allInputRefs((groupIndex, qaIndex)).focus)),
                answersString
              )
            }
          )
        )
    }

    def render(s: State) = {
      WebsocketLoadable(
        WebsocketLoadableProps(
          websocketURI = websocketUri,
          request = QASRLGenerationApiRequest(workerIdOpt, prompt),
          onLoad = ((response: QASRLGenerationApiResponse) => scope.setState(State.initFromResponse(response))),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Loading => <.div("Retrieving data...")
            case Loaded(QASRLGenerationApiResponse(GenerationStatSummary(numVerbsCompleted, numQuestionsWritten, workerStatsOpt), sentence, _), _) =>
              val questionsPerVerbOpt = if(numVerbsCompleted == 0) None else Some(
                numQuestionsWritten.toDouble / numVerbsCompleted
              )
              val remainingInCoverageGracePeriodOpt = questionsPerVerbOpt
                .as(QASRLSettings.generationCoverageGracePeriod - numVerbsCompleted)
                .filter(_ > 0)

              val accuracyOpt = workerStatsOpt.map(_.accuracy)
              val remainingInAccuracyGracePeriodOpt = for {
                workerStats <- workerStatsOpt
                remaining = QASRLSettings.generationAccuracyGracePeriod - workerStats.numAssignmentsCompleted
                if remaining > 0
              } yield remaining

              SpanHighlighting(
                SpanHighlightingProps(
                  isEnabled = !isNotAssigned,
                  update = updateHighlights,
                  render = {
                    case (SpanHighlightingState(spans, status), SpanHighlightingContext(setSpan, hover, touch, cancelHighlight)) =>

                      val curCompleteQAPairs = getAllCompleteQAPairs(s.qaGroups)

                      val curPotentialBonus = QASRLSettings.generationBonus(curCompleteQAPairs.size)
                      val nextPotentialBonus = QASRLSettings.generationBonus(curCompleteQAPairs.size + 1) - curPotentialBonus

                      // val groupAnswersByQAIndex = s.curFocus.foldMap {
                      //   case (groupIndex, _) => s.qaGroups(groupIndex).qas.indices.map(qaIndex =>
                      //     qaIndex -> spans((groupIndex, qaIndex)).map(_.indices)
                      //   ).toMap
                      // }

                      val otherAnswerSpansInGroup = s.curFocus.foldMap {
                        case (groupIndex, qaIndex) =>
                          s.qaGroups(groupIndex).qas.indices
                            .filterNot(_ == qaIndex)
                            .flatMap(otherQAIndex => spans(groupIndex, otherQAIndex))
                            .toList
                      }

                      // val otherAnswerWordsInGroup = otherAnswersInGroup.flatten.toSet

                      val curAnswerSpans = s.curFocus.foldMap(spans)

                      val inProgressAnswerOpt = SpanHighlightingStatus.highlighting.getOption(status).map {
                        case Highlighting(_, anchor, endpoint) => ContiguousSpan(anchor, endpoint)
                      }

                      def hoverWord(i: Int) = s.curFocus.foldMap {
                        case (groupIndex, qaIndex) => hover((groupIndex, qaIndex))(i)
                      }

                      def touchWord(i: Int) = s.curFocus.foldMap {
                        case (groupIndex, qaIndex) => touch((groupIndex, qaIndex))(i)
                      }

                      def ifCurGroup[M: Monoid](groupIndex: Int)(valueIfCorrect: M): M =
                        s.curFocus.filter(_._1 == groupIndex).foldMap(_ => valueIfCorrect)

                      <.div(
                        ^.classSet1("container-fluid"),
                        ^.onClick --> cancelHighlight,
                        Styles.mainContent,
                        <.div(
                          ^.margin := "5px",
                          instructions
                        ),
                        questionsPerVerbOpt.whenDefined(questionsPerVerb =>
                          <.div(
                            ^.classSet1("card"),
                            ^.margin := "5px",
                            ^.padding := "5px",
                            <.p(
                              """So far, you have written """,
                              <.span(
                                if(questionsPerVerb <= QASRLSettings.generationCoverageQuestionsPerVerbThreshold) {
                                  Styles.badRed
                                } else if(questionsPerVerb <= (QASRLSettings.generationCoverageQuestionsPerVerbThreshold * 1.1)) {
                                  TagMod(Styles.uncomfortableOrange, Styles.bolded)
                                } else {
                                  Styles.goodGreen
                                },
                                f"$questionsPerVerb%.1f"
                              ),
                              " questions per verb. This must remain above 2.0",
                              remainingInCoverageGracePeriodOpt.fold(".")(remaining =>
                                s" after the end of the grace period ($remaining verbs remaining)."
                              )
                            ),
                            accuracyOpt.whenDefined(accuracy =>
                              <.p(
                                """Of your questions that have been validated, """,
                                <.span(
                                  if(accuracy <= QASRLSettings.generationAccuracyBlockingThreshold) {
                                    Styles.badRed
                                  } else if(accuracy <= QASRLSettings.generationAccuracyBlockingThreshold + 0.05) {
                                    TagMod(Styles.uncomfortableOrange, Styles.bolded)
                                  } else {
                                    Styles.goodGreen
                                  },
                                  f"${accuracy * 100.0}%.1f%%"
                                ),
                                f""" were judged valid by other annotators. This must remain above ${QASRLSettings.generationAccuracyBlockingThreshold * 100.0}%.1f%%""",
                                remainingInAccuracyGracePeriodOpt.fold(".")(remaining =>
                                  s" after the end of the grace period ($remaining verbs remaining)."
                                )
                              )
                            )
                          )
                        ),
                        <.div(
                          ^.classSet1("card"),
                          ^.margin := "5px",
                          ^.padding := "5px",
                          (0 until s.qaGroups.size).toVdomArray(groupIndex =>
                            <.div(
                              ^.marginBottom := "20px",
                              MultiContigSpanHighlightableSentence(
                                MultiContigSpanHighlightableSentenceProps(
                                  sentence = sentence,
                                  styleForIndex = i => TagMod(Styles.specialWord, Styles.niceBlue).when(i == prompt.keywords(groupIndex)),
                                  highlightedSpans = (
                                    inProgressAnswerOpt.map(_ -> (^.backgroundColor := "#FF8000")) ::
                                      (curAnswerSpans.map(_ -> (^.backgroundColor := "#FFFF00")) ++
                                         otherAnswerSpansInGroup.map(_ -> (^.backgroundColor := "#DDDDDD"))).map(Some(_))).flatten,
                                  hover = hoverWord,
                                  touch = touchWord,
                                  render = (elements =>
                                    <.div(
                                      ^.onMouseEnter --> setBlurEnabled(false),
                                      ^.onMouseLeave --> setBlurEnabled(true),
                                      <.p(
                                        Styles.largeText,
                                        Styles.unselectable,
                                        elements.toVdomArray)
                                    )
                                  ))
                              ),
                              <.ul(
                                Styles.listlessList,
                                (0 until s.qaGroups(groupIndex).qas.size).toVdomArray(qaIndex =>
                                  <.li(
                                    ^.display := "block",
                                    qaField(s, sentence, groupIndex, qaIndex, nextPotentialBonus)
                                  )
                                )
                              )
                            )
                          ),
                          <.p(
                            ^.marginTop := "5px",
                            "Potential bonus so far: ",
                            <.span(
                              Styles.goodGreen.when(curPotentialBonus > 0),
                              Styles.bolded.when(curPotentialBonus > 0),
                              s"${math.round(100 * curPotentialBonus).toInt}c"
                            )
                          )
                        ),
                        <.div(
                          ^.classSet1("form-group"),
                          ^.margin := "5px",
                          <.textarea(
                            ^.classSet1("form-control"),
                            ^.name := FieldLabels.feedbackLabel,
                            ^.rows := 3,
                            ^.placeholder := "Feedback? (Optional)"
                          )
                        ),
                        <.input(
                          ^.classSet1("btn btn-primary btn-lg btn-block"),
                          ^.margin := "5px",
                          ^.`type` := "submit",
                          ^.disabled := isNotAssigned || !s.qaGroups.forall(getCompleteQAPairs(_).size > 0),
                          ^.id := FieldLabels.submitButtonLabel,
                          ^.value := (
                            if(isNotAssigned) "You must accept the HIT to submit results"
                            else if(!s.qaGroups.forall(getCompleteQAPairs(_).size > 0)) "You must write and answer at least one question to submit results"
                            else "Submit"
                          )
                        )
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
