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
  preTaskInstructions: VdomTag,
  postTaskInstructions: VdomTag)(
  implicit promptReader: Reader[GenerationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseWriter: Writer[List[VerbQA]] // same as above
) extends TaskClient[GenerationPrompt[SID], List[VerbQA]] {

  // TODO put in TaskClient
  lazy val workerIdOpt: Option[String] = {
    import scala.scalajs.js.Dynamic.global
    Option(global.turkGetParam("workerId", "UNASSIGNED").asInstanceOf[String]).filter(_ != "UNASSIGNED")
  }

  // for monoid on Callback
  implicit def appMonoid[F[_]: Applicative, A: Monoid] = Applicative.monoid[F, A]

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val WebsocketLoadableComponent = new WebsocketLoadableComponent[QASRLGenerationApiRequest[SID], QASRLGenerationApiResponse]
  import WebsocketLoadableComponent._
  val HighlightingComponent = new HighlightingComponent[(Int, Int, Int, Int)] // keyword index AMONG KEYWORDS, qa index, answer index, answer word index
  import HighlightingComponent._

  val IntState = new LocalStateComponent[Int]

  val ReferenceComponent = new ReferenceComponent[dom.html.Element]
  import ReferenceComponent._

  import MultiSpanHighlightableSentenceComponent._

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
    curFocus: Option[(Int, Int, Int)]) {
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

  // def getQAState(template: QASRLStatefulTemplate, qa: QAPair): QAState =
  //   template.processStringFully(qa.question) match {
  //     case Left(QASRLStatefulTemplate.AggregatedInvalidState(_, numGoodCharacters)) => Invalid
  //     case Right(validStates) => if(validStates.exists(_.isComplete)) Complete else InProgress
  //   }

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
        val qaFocus = (focus._1, focus._2)
        val allFocusablePairs = s.qaGroups.toVector.zipWithIndex.flatMap {
          case (group, groupIndex) => group.qas.zipWithIndex.map {
            case (_, qaIndex) => (groupIndex, qaIndex)
          }
        }
        val curIndex = allFocusablePairs.indexOf(qaFocus)
        val newIndex = (curIndex + 1) % allFocusablePairs.size

        Callback(allInputRefs(allFocusablePairs(newIndex)).focus)
      }
    }

    def moveToAnswer(targetAnswerIndex: Int): Callback = scope.modState(
      (State.curFocus composePrism OptionOptics.some composeLens Optics.third).set(targetAnswerIndex)
    )

    def updateHighlights(hs: HighlightingState) =
      scope.modState(
        State.qaGroups.composeTraversal(eachIndexed).modify {
          case (group, groupIndex) => QAGroup.qas.composeTraversal(eachIndexed).modify {
            case (qa, qaIndex) => QAPair.answers.set(
              hs.span.collect {
                case (`groupIndex`, `qaIndex`, answerIndex, wordIndex) => (answerIndex, wordIndex)
              }.groupBy(_._1).toList.sortBy(_._1).map(_._2.map(p => p._2).toSet))(qa) -> qaIndex
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
        val answerFocus = curFocus match {
          case Some((`groupIndex`, `qaIndex`, answerIndex)) => Some(answerIndex)
          case _ => None
        }
        val isFocused = answerFocus.nonEmpty
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
                  Reference(
                    ReferenceProps(
                      referencedTag = <.input(
                        (^.disabled := true).when(isNotAssigned),
                        bgStyle,
                        ^.float := "left",
                        ^.`type` := "text",
                        ^.placeholder := (
                          if(qaIndex == 0) ("Question about \"" + Text.normalizeToken(sentence(qaGroup.verbIndex)) + "\" (required)")
                          else ("Question about \"" + Text.normalizeToken(sentence(qaGroup.verbIndex)) + "\"" + s" (+${math.round(100 * nextPotentialBonus).toInt}c)")
                        ),
                        ^.margin := s"1px",
                        ^.padding := s"1px",
                        ^.width := "480px",
                        ^.onKeyDown ==> handleKey,
                        ^.onChange ==> ((e: ReactEventFromInput) => handleQuestionChange(e.target.value)),
                        ^.onFocus --> scope.modState(
                          State.curFocus.modify {
                            case cf @ Some((`groupIndex`, `qaIndex`, answerIndex)) => cf
                            case _ => Some((groupIndex, qaIndex, 0))
                          }),
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
                                 ^.background := "rgba(255, 255, 255, 1.0)",
                                 ^.padding := "2px 0",
                                 ^.fontSize := "90%",
                                 ^.overflow := "auto",
                                 ^.maxHeight := "50%",
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
                      }
                    )
                  )
                )
              })),
          <.div(
            ^.float := "left",
            ^.margin := "1px",
            ^.padding := "1px",
            ^.onMouseMove --> setBlurEnabled(false),
            ^.onMouseLeave --> setBlurEnabled(true),
            if(isFocused) {
              if(answers.isEmpty) <.span(
                ^.color := "#CCCCCC",
                "Highlight your answer above"
              ) else (0 to answers.size).map(i =>
                <.span(
                  ^.onClick --> moveToAnswer(i),
                  if(i < answers.size) TagMod(
                    (^.backgroundColor := "#FFFF00").when(answerFocus.nonEmptyAnd(_ == i)),
                    Text.renderSpan(sentence, answers(i))
                  ) else TagMod(
                    (^.color := "#CCCCCC"),
                    if(answerFocus.nonEmptyAnd(_ == i)) TagMod(
                      ^.backgroundColor := "#FFFF00",
                      "Highlight a new answer above, or click other answers to edit"
                    ) else "Click here to add new answer; click other answers to edit"
                  )
                )
              ).toList.map(List(_)).intercalate(List(<.span(" / "))).toVdomArray
            } else <.span(
              (0 until answers.size).map(i =>
                <.span(
                  ^.onClick --> (
                    setBlurEnabled(true) >>
                      Callback(allInputRefs((groupIndex, qaIndex)).focus) >>
                      moveToAnswer(i) >>
                      setBlurEnabled(false)
                  ),
                  Text.renderSpan(sentence, answers(i))
                )
              ).toList.map(List(_)).intercalate(List(<.span(" / "))).toVdomArray
            )
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
              val remainingInCoverageGracePeriod = QASRLSettings.generationCoverageGracePeriod - numVerbsCompleted

              val accuracyOpt = workerStatsOpt.map(_.accuracy)
              val remainingInAccuracyGracePeriodOpt = for {
                workerStats <- workerStatsOpt
                warnedAt <- workerStats.warnedAt
              } yield math.max(0, QASRLSettings.generationBufferBeforeBlocking + warnedAt - workerStats.numAssignmentsCompleted)

              Highlighting(
                HighlightingProps(
                  isEnabled = !isNotAssigned,
                  preUpdate = HighlightingState.span.modify(
                    span => span.map(t => (t._1, t._2)).flatMap {
                      case (keywordIndex, qaIndex) => span.collect {
                        case (`keywordIndex`, `qaIndex`, answerIndex, wordIndex) => (answerIndex, wordIndex)
                      }.groupBy(_._1).toList.sortBy(_._1).map(_._2.map(p => p._2)).zipWithIndex.flatMap {
                        case (wordIndices, newAnswerIndex) => wordIndices.map((keywordIndex, qaIndex, newAnswerIndex, _))
                      }.toSet
                    }
                  ),
                  update = updateHighlights,
                  render = {
                    case (HighlightingState(spans, status),
                          HighlightingContext(setSpan, startHighlight, startErase, stopHighlight, touchElement)) =>

                      val curCompleteQAPairs = getAllCompleteQAPairs(s.qaGroups)

                      val curPotentialBonus = QASRLSettings.generationBonus(curCompleteQAPairs.size)
                      val nextPotentialBonus = QASRLSettings.generationBonus(curCompleteQAPairs.size + 1) - curPotentialBonus

                      val groupAnswersByQAIndex = s.curFocus.foldMap {
                        case (groupIndex, _, _) => spans.collect {
                          case (`groupIndex`, qaIndex, answerIndex, wordIndex) => (qaIndex, answerIndex, wordIndex)
                        }
                      }.groupBy(_._1).map {
                        case (qaIndex, tuples) => qaIndex -> tuples
                            .map(t => (t._2, t._3))
                            .groupBy(_._1).toList.sortBy(_._1)
                            .map(_._2).toList.map(_.map(p => p._2).toSet)
                      }

                      val otherAnswersInGroup = s.curFocus.foldMap {
                        case (_, qaIndex, answerIndex) =>
                          groupAnswersByQAIndex.toList.flatMap {
                            case (`qaIndex`, answerSpans) => answerSpans.take(answerIndex) ++ answerSpans.drop(answerIndex + 1)
                            case (_, answerSpans) => answerSpans
                          }
                      }

                      val otherAnswerWordsInGroup = otherAnswersInGroup.flatten.toSet

                      val curAnswers = s.curFocus.flatMap {
                        case (_, qaIndex, _) => groupAnswersByQAIndex.get(qaIndex)
                      }.foldK

                      val curAnswer = s.curFocus.foldMap {
                        case (_, _, answerIndex) => curAnswers.lift(answerIndex).foldMap(identity)
                      }

                      // val otherAnswersToQuestion = s.curFocus.foldMap {
                      //   case (_, _, answerIndex) => curAnswers.take(answerIndex) ++ curAnswers.drop(answerIndex + 1)
                      // }

                      def touchWord(i: Int) = s.curFocus.foldMap {
                        case (groupIndex, qaIndex, answerIndex) => touchElement((groupIndex, qaIndex, answerIndex, i))
                      }

                      def ifCurGroup[M: Monoid](groupIndex: Int)(valueIfCorrect: M): M =
                        s.curFocus.filter(_._1 == groupIndex).foldMap(_ => valueIfCorrect)

                      <.div(
                        ^.classSet1("container-fluid"),
                        ^.onMouseUp --> stopHighlight,
                        ^.onMouseDown --> startHighlight,
                        Styles.mainContent,
                        preTaskInstructions,
                        <.hr(),
                        questionsPerVerbOpt.whenDefined(questionsPerVerb =>
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
                            if(remainingInCoverageGracePeriod > 0) s" after the end of the grace period ($remainingInCoverageGracePeriod verbs remaining)."
                            else "."
                          )
                        ),
                        accuracyOpt.whenDefined(accuracy =>
                          <.p(
                            """Of your questions that have been validated, """,
                            <.span(
                              if(accuracy <= QASRLSettings.generationAccuracyBlockingThreshold) {
                                Styles.badRed
                              } else if(accuracy <= QASRLSettings.generationAccuracyWarningThreshold) {
                                TagMod(Styles.uncomfortableOrange, Styles.bolded)
                              } else {
                                Styles.goodGreen
                              },
                              f"${accuracy * 100.0}%.1f%%"
                            ),
                            f""" were judged valid by other annotators. This must remain above ${QASRLSettings.generationAccuracyBlockingThreshold * 100.0}%.1f%%""",
                            remainingInAccuracyGracePeriodOpt.fold(".")(remaining =>
                              if(remaining > 0) s" after the end of a grace period ($remaining verbs remaining)."
                              else " (no grace period remaining)."
                            )
                          )
                        ),
                        <.div(
                          (0 until s.qaGroups.size).toVdomArray(groupIndex =>
                            <.div(
                              ^.marginBottom := "20px",
                              MultiSpanHighlightableSentence(
                                MultiSpanHighlightableSentenceProps(
                                  sentence = sentence,
                                  styleForIndex = ((i: Int) =>
                                    TagMod(Styles.specialWord, Styles.niceBlue).when(
                                      i == prompt.keywords(groupIndex)
                                    )
                                  ),
                                  highlightedSpans = (curAnswer, ^.backgroundColor := "#FFFF00") :: otherAnswersInGroup.map(
                                    (_, ^.backgroundColor := "#DDDDDD")
                                  ),
                                  startHighlight = startHighlight,
                                  startErase = startErase,
                                  touchWord = (i: Int) => if(otherAnswerWordsInGroup.contains(i)) Callback.empty else touchWord(i),
                                  render = (elements =>
                                    <.div(
                                      ^.onMouseEnter --> setBlurEnabled(false),
                                      ^.onMouseLeave --> setBlurEnabled(true),
                                      <.blockquote(
                                        ^.classSet1("blockquote"),
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
                        postTaskInstructions
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
