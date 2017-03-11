package mts.experiments.expH

import mts.experiments._
import mts.datasets.ptb._
import mts.util.dollarsToCents
import mts.tasks._
import mts.language._
import mts.core._
import mts.experiments.expF.WebsocketLoadableComponent

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import upickle.default._

import monocle._
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

object DashboardClient extends TaskClient[Unit, Unit] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    ReactDOM.render(FullUI(), dom.document.getElementById(rootClientDivLabel))
  }

  val WebsocketComponent = new WebsocketComponent[Unit, SummaryInfo]
  import WebsocketComponent._

  @Lenses case class State(
    summaryOpt: Option[SummaryInfo]
  )
  object State {
    def initial = State(None)
  }

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def render(state: State) = {
      Websocket(
        WebsocketProps(
          websocketURI = websocketUri,
          onMessage = (si: SummaryInfo) => scope.modState(State.summaryOpt.set(Some(si))),
          render = {
            case Connecting => <.div("Connecting to server...")
            case Connected(_) => state.summaryOpt.fold(<.div("Waiting for data...")) {
              case summary @ SummaryInfo(
                numGenActive, genWorkerStats, genFeedback,
                numValPromptsWaiting, numValActive, valWorkerInfo, valFeedback,
                lastFewSentences, aggSentenceStats) =>

                val numSentencesCompleted: Int = 0 // TODO from aggSentenceStats

                val estSentenceCompletionRate =
                  if(lastFewSentences.isEmpty) None
                  else Some {
                    val time = lastFewSentences
                      .flatMap(_._2.valHITInfos)
                      .flatMap(_.assignments)
                      .map(_.acceptTime)
                      .max
                    val aggTime = aggSentenceStats.now
                    val deltaMinutes = (aggTime - time) / 1000000000L / 60
                    lastFewSentences.size.toDouble / deltaMinutes
                  }

                <.div(
                  <.h2("Sentences"),
                  <.p(s"Completed: $numSentencesCompleted"),
                  estSentenceCompletionRate.map(r => s"Est. completion rate: $r"),
                  aggSentenceStats match {
                    case AggregateSentenceStats(
                      now,
                      numSentences, numKeywords, numQAPairs, numValidQAPairs,
                      keywordPromptQAPairHist, keywordActualQAPairHist, validationLatencyHist,
                      generationCost, validationCost) =>

                      <.div(
                        <.p(s"Number of sentences completed: $numSentences"),
                        <.p(s"Number of keywords covered: $numKeywords"),
                        <.p(s"Number of QA pairs submitted: $numQAPairs"),
                        <.p(s"Number of QA pairs valid: $numValidQAPairs"),
                        <.p(s"Total cost of generation: $generationCost"),
                        <.p(s"Total cost of validation: $validationCost"),
                        <.p(f"Average cost per sentence: ${(generationCost + validationCost) / numSentences}%.2f"),
                        for {
                          mean <- keywordPromptQAPairHist.mean
                          stdev <- keywordPromptQAPairHist.stdev
                        } yield <.p(f"QA pairs given per keyword prompt: $mean%.2f, stdev $stdev%.2f"),
                        for {
                          mean <- keywordActualQAPairHist.mean
                          stdev <- keywordActualQAPairHist.stdev
                        } yield <.p(f"QA pairs expected to contain a keyword: $mean%.2f, stdev $stdev%.2f"),
                        for {
                          mean <- validationLatencyHist.mean
                          stdev <- validationLatencyHist.stdev
                        } yield <.p(f"Latency from generation to validation (seconds): $mean%.2f, stdev $stdev%.2f")
                      )
                  },
                  lastFewSentences.map {
                    case (sentenceStats, shi @ SentenceHITInfo(_, genHITInfos, valHITInfos)) =>
                      import sentenceStats._
                      <.div(
                        TextRendering.renderSentence(sentence),
                        <.p(s"Num keywords: $numKeywords"),
                        <.p(s"Num QA pairs: $numQAPairs"),
                        <.p(s"Num valid QA pairs: $numValidQAPairs"),
                        <.p(s"Generation cost: $generationCost"),
                        <.p(s"Validation cost: $validationCost"),
                        <.p(s"Validation latencies (s): ${validationLatencies.mkString(", ")}"),
                        <.table(
                          <.tr(
                            List(
                              "Worker ID", "Keyword", "Question", "Answer"
                            ).map(<.td(_))
                          ),
                          for {
                            ValidatedAssignment(genHIT, genAssignment, valAssignments) <- shi.alignValidations
                            validations = valAssignments.map(_.response).transpose
                            (WordedQAPair(keywordIndex, question, answer), qaIndex) <- genAssignment.response.zipWithIndex
                            validationCells = validations(qaIndex).map(va =>
                              <.td(renderValidationAnswer(sentence, va, genAssignment.response)))
                          } yield <.tr(
                            List(
                              genAssignment.workerId,
                              TextRendering.normalizeToken(sentence.words(keywordIndex).token),
                              question, TextRendering.renderSpan(sentence, answer)
                            ).map(<.td(_)),
                            validationCells
                          )
                        )

                      )
                  },
                  <.h2("Generation"),
                  <.p(s"Number of HITs active: $numGenActive"),
                  <.p(s"Recent feedback: ", <.ul(genFeedback.map(<.li(_)))),
                  <.h3("Generation worker stats"),
                  <.table(
                    <.tr(
                      List("Worker ID", "Assignments", "Accuracy",
                           "Earnings", "QA pairs", "Valid QA pairs",
                           "Warning", "Block").map(<.th(_))
                    ),
                    genWorkerStats.values.map {
                      case ws @ WorkerStats(
                        workerId, numAssignmentsCompleted,
                        numQAPairsWritten, numQAPairsValid,
                        earnings, warnedAt, blockedAt) =>

                        <.tr(
                          List(workerId, numAssignmentsCompleted.toString, f"${ws.accuracy}%.3f",
                               f"$earnings%.2f", numQAPairsWritten.toString, numQAPairsValid.toString,
                               warnedAt.fold("")(_.toString), blockedAt.fold("")(_.toString)
                          ).map(<.td(_))
                        )
                    }
                  ),
                  <.h2("Validation"),
                  <.p(s"Number of HITs active: $numValActive"),
                  <.p(s"Number of HITs queued: $numValPromptsWaiting"),
                  <.p(s"Recent feedback: ", <.ul(valFeedback.map(<.li(_)))),
                  <.h3("Validation worker stats"),
                  <.table(
                    <.tr(
                      List("Worker ID", "Assignments", "Earnings",
                           "Agreement rate", "Comparisons", "Agreements",
                           "Answer spans", "Invalids", "Redundants",
                           "Warning", "Block").map(<.th(_))
                    ),
                    valWorkerInfo.values.map {
                      case wi @ WorkerInfo(
                        workerId, numAssignmentsCompleted,
                        numComparisonInstances, numComparisonAgreements,
                        numAnswerSpans, numInvalids, numRedundants,
                        earnings, warnedAt, blockedAt) =>

                        val numTotalAnswers = numAnswerSpans + numInvalids + numRedundants
                        def percent(n: Int) = f"$n%d (${n.toDouble * 100 / numTotalAnswers}%.1f)"

                        <.tr(
                          List(workerId, numAssignmentsCompleted.toString, f"$earnings%.2f",
                               f"${wi.agreement}%.3f", numComparisonInstances.toString, numComparisonAgreements.toString,
                               percent(numAnswerSpans), percent(numInvalids), percent(numRedundants),
                               warnedAt.fold("")(_.toString), blockedAt.fold("")(_.toString)
                          ).map(<.td(_))
                        )
                    }
                  )
                )
            }
          }
        )
      )
    }
  }

  val FullUI = ReactComponentB[Unit]("Full UI")
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .build
}
