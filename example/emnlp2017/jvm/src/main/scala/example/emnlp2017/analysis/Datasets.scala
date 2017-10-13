package example.emnlp2017.analysis

import example.emnlp2017.SentenceId
import example.emnlp2017.loadOutputFile

import turksem.qamr.QAData
import turksem.qamr.SourcedQA
import turksem.qamr.readTSV

import turksem.util.beginsWithWh

object Datasets {
  def readDataTSV(lines: Iterator[String]): QAData[SentenceId] = readTSV(lines, SentenceId.fromString)

  lazy val trainUnfiltered = readDataTSV(loadOutputFile("train.tsv").get.iterator)
  lazy val devUnfiltered = readDataTSV(loadOutputFile("dev.tsv").get.iterator)
  lazy val testUnfiltered = readDataTSV(loadOutputFile("test.tsv").get.iterator)
  lazy val ptbTrainUnfiltered = readDataTSV(loadOutputFile("ptb-train.tsv").get.iterator)
  lazy val ptbDevUnfiltered = readDataTSV(loadOutputFile("ptb-dev.tsv").get.iterator)
  lazy val ptbTestUnfiltered = readDataTSV(loadOutputFile("ptb-test.tsv").get.iterator)
  lazy val ptbAMRUnfiltered = readDataTSV(loadOutputFile("ptb-amr.tsv").get.iterator)

  def isQAGood(sqa: SourcedQA[SentenceId]): Boolean =
    sqa.isValid && beginsWithWh(sqa.question) && !sqa.question.toLowerCase.startsWith("who's")

  lazy val train = trainUnfiltered.filterByQA(isQAGood)
  lazy val dev = devUnfiltered.filterByQA(isQAGood)
  lazy val test = testUnfiltered.filterByQA(isQAGood)
  lazy val ptbTrain = ptbTrainUnfiltered.filterByQA(isQAGood)
  lazy val ptbDev = ptbDevUnfiltered.filterByQA(isQAGood)
  lazy val ptbTest = ptbTestUnfiltered.filterByQA(isQAGood)
  lazy val ptbAMR = ptbAMRUnfiltered.filterByQA(isQAGood)
  lazy val ptbAll = new QAData(ptbTrain.all ++ ptbDev.all ++ ptbTest.all ++ ptbAMR.all)

  lazy val trainDevPTB = new QAData(train.all ++ dev.all ++ ptbAll.all)
}
