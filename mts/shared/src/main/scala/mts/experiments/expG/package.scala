package mts.experiments

import mts.util.LowerCaseStrings._

import mts.conll.CoNLLSentencePath
import mts.conll.CoNLLSentence

import mts.language._

import monocle._
import monocle.macros._

package object expG extends PackagePlatformExtensions {

  sealed trait ValidatedQuestion {
    final def fold[A](invalid: A)(valid: (Int, String) => A): A = this match {
      case InvalidQuestion => invalid
      case EditedQuestion(keywordIndex, newString) => valid(keywordIndex, newString)
    }
    final def overlappingIndices(alignedTokens: Set[(String, Int)]): Set[Int] = this match {
      case InvalidQuestion => Set.empty[Int]
      case EditedQuestion(keywordIndex, newString) =>
        val qTokens = simpleTokenize(newString.toLowerCase).toSet
        val keyTokens = alignedTokens.filter(_._2 == keywordIndex).map(_._1)
        alignedTokens
          .filterNot(p => keyTokens.contains(p._1.toLowerCase))
          .filter(p => qTokens.contains(p._1.toLowerCase))
          .map(_._2)
    }
    final def overlappingWords(alignedTokens: Set[(String, Int)]): Set[String] = this match {
      case InvalidQuestion => Set.empty[String]
      case EditedQuestion(keywordIndex, newString) =>
        val keyTokens = alignedTokens.filter(_._2 == keywordIndex).map(_._1)
        val allTokens = alignedTokens
          .map(_._1)
          .filterNot(keyTokens.contains)
        simpleTokenize(newString.toLowerCase).filter(allTokens.contains).toSet
    }
  }
  case object InvalidQuestion extends ValidatedQuestion
  @Lenses case class EditedQuestion(
    keywordIndex: Int,
    newString: String
  ) extends ValidatedQuestion
  object ValidatedQuestion {
    val invalidQuestion: Prism[ValidatedQuestion, InvalidQuestion.type] = GenPrism[ValidatedQuestion, InvalidQuestion.type]
    val editedQuestion: Prism[ValidatedQuestion, EditedQuestion] = GenPrism[ValidatedQuestion, EditedQuestion]
  }

  case class SourcedTokenizedQAPair(
    originalHITId: String,
    originalIndex: Int,
    questionTokens: Vector[String],
    answer: Set[Int])

  case class TokenizedValidationPrompt(
    path: CoNLLSentencePath,
    sourcedTokenizedQAPairs: List[SourcedTokenizedQAPair])

  case class KeywordQuestionValidationResponse(
    validatedQuestions: List[ValidatedQuestion])

  sealed trait ApiRequest
  case class SentenceRequest(path: CoNLLSentencePath) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(sentence: CoNLLSentence, alignedTokens: Set[(String, Int)]) extends ApiResponse


  sealed trait ManualQAApiRequest
  case object AllRecordsRequest extends ManualQAApiRequest
  case class AllRecordsUpdate(records: List[ManualQARecord]) extends ManualQAApiRequest

  sealed trait ManualQAApiResponse
  case class AllRecordsResponse(paths: List[ManualQARecord]) extends ManualQAApiResponse

  @Lenses case class ManualQARecord(
    path: CoNLLSentencePath,
    sentence: CoNLLSentence,
    qaGroups: List[List[KeywordedQAPair]]
  )
  object ManualQARecord {
    def blank(path: CoNLLSentencePath, sentence: CoNLLSentence) =
      ManualQARecord(path, sentence, List(List(KeywordedQAPair.blank)))
  }

  case class KeywordedQAPair(
    question: String,
    keywordIndex: Int,
    answerIndices: Set[Int]
  )
  object KeywordedQAPair {
    val blank = KeywordedQAPair("", -1, Set.empty[Int])
  }

}
