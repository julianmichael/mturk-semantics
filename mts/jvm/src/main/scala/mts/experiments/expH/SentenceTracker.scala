package mts.experiments.expH
import akka.actor.Actor

import mts.ptb._
import mts.util._
import mts.experiments._
import mts.language._

import upickle.default._

case class GenerationFinished(prompt: GenerationPrompt)
case class ValidationBegun(prompt: ValidationPrompt)
case class ValidationFinished(prompt: ValidationPrompt)

class SentenceTracker extends Actor {

  val finishedSentencesFilename = "finishedSentences"
  var finishedSentences: Set[PTBSentencePath] =
    FileManager.loadDataFile(finalExperimentName, finishedSentencesFilename)
      .map(_.mkString)
      .map(read[Set[PTBSentencePath]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Set.empty[PTBSentencePath]
  }

  val sentenceStatusesFilename = "sentenceStatuses"
  var sentenceStatuses: Map[PTBSentencePath, SentenceStatus] =
    FileManager.loadDataFile(finalExperimentName, sentenceStatusesFilename)
      .map(_.mkString)
      .map(read[Map[PTBSentencePath, SentenceStatus]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[PTBSentencePath, SentenceStatus]
    }

  def saveData = {
    FileManager.saveDataFile(
      finalExperimentName,
      finishedSentencesFilename,
      write[Set[PTBSentencePath]](finishedSentences))
    FileManager.saveDataFile(
      finalExperimentName,
      sentenceStatusesFilename,
      write[Map[PTBSentencePath, SentenceStatus]](sentenceStatuses))
  }

  case class SentenceStatus(
    path: PTBSentencePath,
    keywordsFinished: Set[Int],
    ongoingValidations: Set[ValidationPrompt]) {
    def isFinished = {
      val sentence = getPTBTokens(path)
      val keywords = sentence.indices
        .filter(i => !reallyUninterestingTokens.contains(sentence(i)))
        .toSet
      val keywordsRemaining = keywords -- keywordsFinished
      keywordsRemaining.isEmpty && ongoingValidations.isEmpty
    }

    def withKeywords(keywords: Set[Int]) = this.copy(
      keywordsFinished = this.keywordsFinished ++ keywords
    )

    def beginValidation(vPrompt: ValidationPrompt) = this.copy(
      ongoingValidations = this.ongoingValidations + vPrompt
    )

    def finishValidation(vPrompt: ValidationPrompt) = this.copy(
      ongoingValidations = this.ongoingValidations - vPrompt
    )
  }
  object SentenceStatus {
    def empty(path: PTBSentencePath) =
      SentenceStatus(path, Set.empty[Int], Set.empty[ValidationPrompt])
  }

  def processUpdate(path: PTBSentencePath, mod: SentenceStatus => SentenceStatus) = {
    val newStatus = {
      val res = sentenceStatuses
        .get(path)
        .getOrElse(SentenceStatus.empty(path))
      mod(res)
    }

    if(newStatus.isFinished) {
      finishedSentences = finishedSentences + path
      sentenceStatuses = sentenceStatuses - path
    } else {
      sentenceStatuses = sentenceStatuses.updated(path, newStatus)
    }
  }

  override def receive = {
    case Reflect => sender ! this // for debugging
    case SaveData => saveData
    case GenerationFinished(gPrompt) => processUpdate(gPrompt.path, _.withKeywords(gPrompt.keywords.toSet))
    case ValidationBegun(vPrompt) => processUpdate(vPrompt.genPrompt.path, _.beginValidation(vPrompt))
    case ValidationFinished(vPrompt) => processUpdate(vPrompt.genPrompt.path, _.finishValidation(vPrompt))
  }
}

