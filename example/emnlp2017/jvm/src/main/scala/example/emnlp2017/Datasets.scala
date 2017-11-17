package example.emnlp2017

import cats.implicits._

import turksem.qamr.QAData
import turksem.qamr.SourcedQA
import turksem.qamr.readTSV

import turksem.util.beginsWithWh
import turksem.util.PosTagger
import turksem.util.Tokenizer

import nlpdata.util.PosTags
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.HasTokens.ops._

object Datasets {

  def readDataTSV(lines: Iterator[String]): QAData[SentenceId] = readTSV(lines, SentenceId.fromString)

  lazy val trainUnfiltered = readDataTSV(loadOutputFile("train.tsv").get.iterator)
  lazy val devUnfiltered = readDataTSV(loadOutputFile("dev.tsv").get.iterator)
  lazy val testUnfiltered = readDataTSV(loadOutputFile("test.tsv").get.iterator)
  lazy val ptbTrainUnfiltered = readDataTSV(loadOutputFile("ptb-train.tsv").get.iterator)
  lazy val ptbDevUnfiltered = readDataTSV(loadOutputFile("ptb-dev.tsv").get.iterator)
  lazy val ptbTestUnfiltered = readDataTSV(loadOutputFile("ptb-test.tsv").get.iterator)
  lazy val ptbAMRUnfiltered = readDataTSV(loadOutputFile("ptb-amr.tsv").get.iterator)


  def isWhatNounQuestion(sqa: SourcedQA[SentenceId]): Boolean = {
    Tokenizer.tokenize(sqa.question).map(_.toLowerCase).toList match {
      case "what" :: potentialNoun :: "?" :: Nil =>
        val allForms = inflections.getAllForms(potentialNoun.lowerCase)
        PosTagger.posTag(sqa.id.sentenceId.tokens).filter(ptToken =>
          allForms.contains(ptToken.token.lowerCase)
        ).forall(ptToken => PosTags.nounPosTags.contains(ptToken.pos))
      case _ => false
    }
  }

  def isQAGood(sqa: SourcedQA[SentenceId]): Boolean =
    sqa.isValid && beginsWithWh(sqa.question) && !sqa.question.toLowerCase.startsWith("who's") && !isWhatNounQuestion(sqa)

  lazy val train = trainUnfiltered.filterByQA(isQAGood)
  lazy val dev = devUnfiltered.filterByQA(isQAGood)
  lazy val test = testUnfiltered.filterByQA(isQAGood)
  lazy val ptbTrain = ptbTrainUnfiltered.filterByQA(isQAGood)
  lazy val ptbDev = ptbDevUnfiltered.filterByQA(isQAGood)
  lazy val ptbTest = ptbTestUnfiltered.filterByQA(isQAGood)
  lazy val ptbAMR = ptbAMRUnfiltered.filterByQA(isQAGood)
  lazy val ptbAll = new QAData(ptbTrain.all ++ ptbDev.all ++ ptbTest.all ++ ptbAMR.all)

  lazy val trainDevPTB = new QAData(train.all ++ dev.all ++ ptbAll.all)
  lazy val allWiki1k = new QAData(train.all ++ dev.all ++ test.all)

  lazy val all = new QAData(trainDevPTB.all ++ test.all)
}
