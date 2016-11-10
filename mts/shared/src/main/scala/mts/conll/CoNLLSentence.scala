package mts.conll

import mts.util._
import scala.util.Try

/** Represents an annotated sentence from the CoNLL data.
  *
  * As of now, we're lazy-loading the members of this class,
  * only implementing them as we need them.
  * I believe coref spans are already implemented in the coref annotation project;
  * if necessary ask me (Julian) and I can put them in.
  */
case class CoNLLSentence(
  // partNum: Int,
  sentenceNum: Int,
  words: List[CoNLLWord],
  syntaxTree: SyntaxTree,
  predicateArgumentStructures: List[PredicateArgumentStructure]
  // nerSpans: Nothing, // TODO
  // corefSpans: List[CorefSpan] // TODO
)

/** Provides parsing of CoNLL sentences.
  *
  * This will grow as the number of fields of CoNLLSentence grows.
  */
object CoNLLSentence {

  /** Reads a CoNLLSentence from a list of lines from a CoNLLFile.
    *
    * Does not expect empty lines on either end of the list.
    * Assumes the lines are taken from a CoNLL data file,
    * undefined behavior otherwise.
    *
    * @param sentenceNum the index of the sentence in the document
    * @param lines the lines of the file containing the sentence's info
    * @return the CoNLL sentence stored in the data
    */
  def readFromLines(sentenceNum: Int, lines: List[String]): CoNLLSentence = {
    val lineArrays = lines.map(_.split("\\s+"))
    val words = lineArrays.map(arr => CoNLLWord(arr(2).toInt, arr(3), arr(4)))
    val treeString = lineArrays.map(arr => arr(5)).mkString
    val tree = SyntaxTree.fromString(treeString, words)
    val predicates = for {
      (arr, index) <- lineArrays.zipWithIndex
      predicateLemma = arr(6)
      if !predicateLemma.equals("-")
      framesetId <- Try(arr(7).toInt).toOption
      head = words(index)
    } yield Predicate(head, predicateLemma, framesetId)
    val paStructures = for {
      (pred, num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
      spansString = lineArrays.map(arr => arr(11 + num)).mkString
      argSpans = ArgumentSpan.fromString(spansString, words)
    } yield PredicateArgumentStructure(pred, argSpans)
    CoNLLSentence(sentenceNum, words, tree, paStructures)
  }
}

/** Represents a single word in a CoNLL sentence.
  *
  * @param index the index of the word in the sentence
  * @param token the orthography of the word (escaped PTB style)
  * @param pos the part of speech of the word (PTB style)
  */
case class CoNLLWord(
  index: Int,
  token: String,
  pos: String)

/** Represents a unique index to a CoNLL sentence.
  *
  * This can be used to easily serialize a sentence without worrying about the data definition changing.
  * The FileManager extension methods for the conll package include one to retrieve a sentence directly
  * from such a path.
  *
  * @param filePath the path to the CoNLL file containing this sentence
  * @param sentenceNum the index of this sentence in the document
  */
case class CoNLLSentencePath(
  filePath: CoNLLPath,
  sentenceNum: Int
) {
  override def toString = s"${filePath.get}:$sentenceNum"
}
