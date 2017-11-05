package example.emnlp2017

import turksem.qamr._
import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import argonaut._
import Argonaut._

object SquadAnalysis {

  case class Subspan(begin: Int, end: Int) {
    def getSubstring(string: String) = string.substring(begin, end)
  }

  case class QAInstance(
    context: String,
    question: String,
    predictedSpan: Subspan,
    answerSpans: List[Subspan]) {
    def predictedString = predictedSpan.getSubstring(context)
    def answerStrings = answerSpans.map(_.getSubstring(context))

    def missed = answerSpans.forall {
      case Subspan(begin, end) => predictedSpan.end < begin || predictedSpan.begin > end
    }

    def prettyString: String = {
      s"$context \n\n$question \n\nPrediction: $predictedString \n\nAnswers:\n" +
        answerStrings.mkString("\n") + "\n"
    }
  }

  def readForSquadTrained = {
    readSquadData("squad-dev-answers.json")
  }

  // def readForQAMRTrained = {
  //   readSquadData("squad-dev-answers-pooled-trained.json")
  // }

  // map from ID to answer string
  def readSquadPredictions(predictionsFilename: String): Map[String, String] = {
    val jsonStr = loadInputFile(predictionsFilename).get.mkString
    Parse.parseOption(jsonStr).get.objectOrEmpty.toMap.map {
      case (k, v) => k -> v.string.get.toString
    }
  }

  def readSquadData(predictionsFilename: String) = {
    val predictions = readSquadPredictions(predictionsFilename)
    val squadJson = Parse.parseOption(loadInputFile("squad-dev-v1.1.json").get.mkString).get
    val dataArray = squadJson.field("data").get.array.get
    dataArray.iterator.flatMap { file =>
      file.field("paragraphs").get.array.get.iterator.flatMap { paragraph =>
        val context = paragraph.field("context").get.string.get.toString
        paragraph.field("qas").get.array.get.map { qa =>
          val id = qa.field("id").get.string.get.toString
          val question = qa.field("question").get.string.get.toString
          val answers = qa.field("answers").get.array.get.iterator.map { answer =>
            val begin = answer.field("answer_start").get.number.get.toInt.get
            val end = begin + answer.field("text").get.string.get.toString.length
            Subspan(begin, end)
          }.toList
          val prediction = {
            val predAnswerString = predictions(id)
            val predBegin = context.indexOf(predAnswerString)
            val predEnd = predBegin + predAnswerString.length
            Subspan(predBegin, predEnd)
          }
          QAInstance(context, question, prediction, answers)
        }
      }
    }.toList

  }

}
