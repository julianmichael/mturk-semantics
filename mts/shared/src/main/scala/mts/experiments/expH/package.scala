package mts.experiments

import mts.ptb.PTBSentencePath

package object expH extends PackagePlatformExtensions {
  val finalExperimentName = "h_final"

  def bonusFor(i: Int): Double = 0.01 * i + 0.04
  val numKeywords = 4
  val questionCharLimit = 50

  val validationBonusPerQuestion = 0.03

  val generationAccuracyThreshold = 0.8
  val generationBufferBeforeWarning = 20
  val generationBufferBeforeBlocking = 10

  val validationAgreementThreshold = 0.75
  val validationBufferBeforeWarning = 15
  val validationBufferBeforeBlocking = 10

  case class GenerationPrompt(
    path: PTBSentencePath,
    keywords: List[Int])

  // List[WordedQAPair] is response for qa gen
  case class WordedQAPair(wordIndex: Int, question: String, answer: Set[Int])

  case class GenerationApiRequest(path: PTBSentencePath)
  case class GenerationApiResponse(tokens: Vector[String])

  // prompt for validation
  case class ValidationPrompt(
    genPrompt: GenerationPrompt,
    sourceHITId: String,
    sourceAssignmentId: String,
    qaPairs: List[WordedQAPair]
  ) {
    def path = genPrompt.path
  }

  // List[ValidationAnswer] is response for validation
  sealed trait ValidationAnswer {
    def isInvalid = this match {
      case InvalidQuestion => true
      case _ => false
    }

    def getRedundant = this match {
      case r @ Redundant(_) => Some(r)
      case _ => None
    }
    def isRedundant = !getRedundant.isEmpty

    def getAnswer = this match {
      case a @ Answer(_) => Some(a)
      case _ => None
    }
    def isAnswer = !getAnswer.isEmpty

    def isComplete = this match {
      case InvalidQuestion => true
      case Redundant(_) => true
      case Answer(indices) => !indices.isEmpty
    }
  }
  case object InvalidQuestion extends ValidationAnswer
  case class Redundant(other: Int) extends ValidationAnswer
  case class Answer(indices: Set[Int]) extends ValidationAnswer

  case class ValidationApiRequest(path: PTBSentencePath)
  case class ValidationApiResponse(sentence: Vector[String])
}
