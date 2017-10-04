package example.tqa

import cats.implicits._

import nlpdata.structure.AlignedToken
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import turksem.util.AligningTokenizer
import turksem.util._

import java.nio.file.Path

class TQAFileSystemTopicTextService(location: Path) {
  val topicTexts = {
    import argonaut._
    import Argonaut._

    val sentencesTrimmedJsonArray = Parse.parse(
      io.Source.fromFile(location.toString).mkString
    ).right.get.array.get

    case class Sentence(
      topicGlobalId: String,
      lessonName: String,
      topicName: String,
      index: Int,
      text: String)

    val trimmedSentences = sentencesTrimmedJsonArray.iterator
      .map(obj => Sentence(
             topicGlobalId = obj.field("globalID").get.string.get.toString,
             lessonName = obj.field("lessonName").get.string.get.toString,
             topicName = obj.field("topicName").get.string.get.toString,
             index = obj.field("sentenceIndex").get.number.get.toInt.get,
             text = obj.field("text").get.string.get.toString))
      .toList

    trimmedSentences.groupBy(_.topicGlobalId).map {
      case (topicId, sentences) =>
        val sortedSentences = sentences.sortBy(_.index)
        sortedSentences.zip(0 until sortedSentences.size).collect {
          case (sentence, index) if sentence.index != index =>
            println(s"missing sentence-index pair: sentence ${sentence.index} at index $index")
        }.toList
        val sortedSentenceTokens = sortedSentences.map { sentence =>
          val tokens = AligningTokenizer.tokenize(sentence.text)
          val roundTrip = Text.renderAligned(tokens)
          val firstBadIndex = roundTrip.zip(sentence.text).toList.findIndex {
            case (x, y) => x != y
          }
          firstBadIndex match {
            case None => ()
            case Some(i) =>
              println("Sentence discrepancy:")
              println(s"Original:   ${sentence.text}")
              println(s"Round-trip: $roundTrip")
          }
          tokens
        }
        topicId -> TQATopicText(
          topicId = topicId,
          lessonName = sentences.head.lessonName,
          topicName = sentences.head.topicName,
          sentences = sortedSentenceTokens)
    }
  }
}
