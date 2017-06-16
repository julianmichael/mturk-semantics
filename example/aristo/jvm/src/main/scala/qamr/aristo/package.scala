package qamr

import qamr.annotation._

import nlpdata.datasets.wiktionary

import scala.util.Try
import java.nio.file.Paths
import java.nio.file.Files

package object aristo {

  def tokenize(s: String): Vector[String] = qamr.emnlp2017.tokenize(s)

  def sentenceSegmentAndTokenize(s: String): Vector[Vector[String]] = {
    import edu.stanford.nlp.process.DocumentPreprocessor;
    import java.io.{BufferedReader, StringReader}
    import scala.collection.JavaConverters._
    new DocumentPreprocessor(
      new BufferedReader(new StringReader(s))
    ).iterator.asScala.map { tokenListJava =>
      tokenListJava.iterator.asScala.map(_.word).toVector: Vector[String]
    }.toVector

  }

  // def posTag(s: List[String]): List[POSTaggedToken] = qamr.emnlp2017.posTag(s)

  val staticDataPath = Paths.get("static-data/aristo")

  def saveDataFile(name: String, contents: String): Try[Unit] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  def loadDataFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("in")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  // aristo data

  val kbDataSourcePath = "kb-questions.tsv"

  lazy val allKBSentencePairs = {
    val lines = loadDataFile(kbDataSourcePath).get
    lines.iterator.map { line =>
      val sentencesInLine = line.split("\t")
      KBSentencePair(sentencesInLine(0), sentencesInLine(1))
    }.toVector
  }

  lazy val allKBSentenceIds = allKBSentencePairs.indices.flatMap { i =>
    List(KBSentenceId(i, false), KBSentenceId(i, true))
  }

  // math problem data

  val mathProblemSourcePath = "math-questions.json"

  lazy val mathProblemsJson = {
    import argonaut._
    import Argonaut._

    val fileString = loadDataFile(mathProblemSourcePath).get.mkString
    Parse.parse(fileString).right.get.array.get
  }

  // lazy val allMathProblemIds = {
  //   mathProblemsJson.right.get.array.get.iterator.map { problemJson =>
  //     problemJson.fieldOrNull("id").number.get.toInt.get
  //   }.toVector
  // }

  lazy val allMathProblems: Map[Int, MathProblem] = {
    mathProblemsJson.iterator.map { problemJson =>
      val id = problemJson.fieldOrNull("id").number.get.toInt.get
      val questionText = problemJson.fieldOrNull("question").string.get
      val tokenizedQuestionSentences = sentenceSegmentAndTokenize(questionText)
      id -> MathProblem(
        problemJson.fieldOrNull("answer").string.get,
        id,
        tokenizedQuestionSentences.init,
        tokenizedQuestionSentences.last,
        problemJson.fieldOrNull("tags").array.get.iterator.map(_.string.get).toSet
      )
    }.toMap
  }

  val mathProblemSentenceIds: Vector[MathProblemSentenceId] = allMathProblems.flatMap { case (id, mathProblem) =>
    mathProblem.background.indices.map(index => MathProblemSentenceId(id, index))
  }.toVector

  def getTokensForId(id: Ai2SentenceId): Vector[String] = id match {
    case KBSentenceId(sentenceIndex, isKBSentence) =>
      if(isKBSentence) tokenize(allKBSentencePairs(sentenceIndex).kbSentence)
      else tokenize(allKBSentencePairs(sentenceIndex).qaSentence)
    case MathProblemSentenceId(problemIndex, sentenceIndex) =>
      allMathProblems(problemIndex).background(sentenceIndex)
  }

  implicit val ai2SentenceIdHasTokens = new HasTokens[Ai2SentenceId] {
    def getTokens(id: Ai2SentenceId): Vector[String] = getTokensForId(id)
  }

  lazy val kbNonDuplicateSentenceIds = allKBSentenceIds.foldLeft((List.empty[KBSentenceId], Set.empty[String])) {
    case ((idSet, sentenceSet), newId) =>
      val newSentence = getTokensForId(newId).mkString(" ")
      if(sentenceSet.contains(newSentence)) (idSet, sentenceSet)
      else (newId :: idSet, sentenceSet + newSentence)
  }._1.reverse.toVector
}
