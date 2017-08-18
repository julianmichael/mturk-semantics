package turksem.qamr

import turksem.qamr.annotation._
import nlpdata.datasets.wiktionary
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._

import scala.util.Try
import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

package object ai2 {

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

  val staticDataPath = Paths.get("static-data/ai2")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
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
    val lines = loadInputFile(kbDataSourcePath).get
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

    val fileString = loadInputFile(mathProblemSourcePath).get.mkString
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

  implicit object Ai2SentenceIdHasTokens extends HasTokens[Ai2SentenceId] {
    def getTokens(id: Ai2SentenceId): Vector[String] = id match {
      case KBSentenceId(sentenceIndex, isKBSentence) =>
        if(isKBSentence) tokenize(allKBSentencePairs(sentenceIndex).kbSentence)
        else tokenize(allKBSentencePairs(sentenceIndex).qaSentence)
      case MathProblemSentenceId(problemIndex, sentenceIndex) =>
        allMathProblems(problemIndex).background(sentenceIndex)
    }
  }

  lazy val kbNonDuplicateSentenceIds = allKBSentenceIds.foldLeft((List.empty[KBSentenceId], Set.empty[String])) {
    case ((idSet, sentenceSet), newId) =>
      val newSentence = newId.tokens.mkString(" ")
      if(sentenceSet.contains(newSentence)) (idSet, sentenceSet)
      else (newId :: idSet, sentenceSet + newSentence)
  }._1.reverse.toVector

  val resourcePath = Paths.get("resources")

  val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  private[this] val conservativeStopwordFilePath = resourcePath.resolve("english-stop-conservative.txt")

  /** Stopword set from a local file.
    *
    * Not sure where the file came from, but I found it in my old repo
    * from Dan Garrette's undergrad NLP class.
    * I deleted some stopwords that we actually want.
    */
  lazy val conservativeStopwords: Set[LowerCaseString] = {
    import scala.collection.JavaConverters._
    val wordLines = Files.lines(conservativeStopwordFilePath).iterator.asScala.toSet
    (wordLines ++ Set("hm", "uh", "um")).map(_.lowerCase)
  }

  /** (non-normalized as well as normalized PTB tokens.) */
  val punctuation = Set[String](
    ".", ",", "!", "?", ";", ":", "...",
    "''", "\"", "'", "`", "``",
    "#", "--", "-", "–", "—", "%", // PTB dashes, hyphens, en and em dashes
    "−", // minus sign (unicode hex 2122)
    "+", "±", "<", "≤", "≥", ">", "=",
    "^", "@", "|", "&",
    "/.", "/?", "/", "\\",
    ")", "]", "}",
    "(", "[", "{",
    "-RRB-", "-RCB-", "-RSB-",
    "-LRB-", "-LCB-", "-LSB-")

  val contractions = Set("n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d")

  val questionWords = Set("who", "what", "when", "where", "why", "how",
                          "whose", "which", "much", "many")

  // NOTE: can get java code with pronouns from HITL stuff
  // for now, settle with this

  val pronouns = Set(
    "I", "me", "my", "mine",
    "we", "us", "our", "ours",
    "you", "your", "yours",
    "he", "him", "his",
    "she", "her", "hers",
    "it", "its",
    "they", "them", "their",
    "someone", "something",
    "this", "that"
  ).map(_.lowerCase)

  lazy val reallyUninterestingTokens = conservativeStopwords ++ punctuation ++ contractions ++ questionWords

  def isReallyUninteresting(t: String) = reallyUninterestingTokens.contains(t) ||
    reallyUninterestingTokens.contains(t.toLowerCase)

  /** Tokenizes an English string. */
  def tokenize(s: String): Vector[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toVector.map(_.word)
  }

  // pos-tagging

  import edu.stanford.nlp.tagger.maxent.MaxentTagger
  lazy val tagger: MaxentTagger  = new MaxentTagger("resources/corenlp/stanford-postagger-2016-10-31/models/english-left3words-distsim.tagger");

  case class POSTaggedToken(token: String, pos: String)

  /** POS-tags a sequence of tokens. */
  def posTag(s: List[String]): List[POSTaggedToken] = {
    tagger.tagTokenizedString(s.mkString(" ")).split(" ").toList
      .map(_.split("_"))
      .map(s => POSTaggedToken(s(0), s(1)))
  }
}
