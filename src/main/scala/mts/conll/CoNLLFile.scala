package mts.conll

import mts.util._
import scala.util.Try

case class CoNLLPath(get: String)

case class CoNLLSentencePath(
  filePath: CoNLLPath,
  sentenceNum: Int
)

case class CoNLLFile(
  id: String,
  sentences: Vector[CoNLLSentence]
)

// see http://conll.cemantix.org/2012/data.html for data format
object CoNLLFile {
  def readFromLines(lines: Iterator[String]): CoNLLFile = {
    val firstLine = lines.next
    val firstLineRegex = """#begin document \((.*)\); part [0-9]+""".r
    val firstLineRegex(id) = firstLine
    val (sentences, _, _) = lines
      .takeWhile(!_.equals("#end document"))
      .foldLeft((List.empty[CoNLLSentence], List.empty[String], 0)) {
      case ((prevSentences, curLines, sentenceNum), line) =>
        if(line.trim.isEmpty) {
          (CoNLLSentence.readFromLines(sentenceNum, curLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
        } else {
          (prevSentences, line :: curLines, sentenceNum)
        }
    }
    CoNLLFile(id, sentences.toVector.reverse)
  }
}

// TODO: speaker/author information

case class CoNLLSentence(
  // partNum: Int,
  sentenceNum: Int,
  words: List[CoNLLWord],
  syntaxTree: SyntaxTree,
  predicateArgumentStructures: List[PredicateArgumentStructure]
  // nerSpans: Nothing, // TODO
  // corefSpans: List[CorefSpan] // TODO
)

object CoNLLSentence {
  def readFromLines(sentenceNum: Int, lines: List[String]): CoNLLSentence = {
    val lineArrays = lines.map(_.split("\\s+"))
    val words = lineArrays.map(arr => CoNLLWord(arr(2).toInt, arr(3), arr(4)))
    val treeString = lineArrays.map(arr => arr(5)).mkString
    val tree = SyntaxTree.fromString(treeString, words).get
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
      argSpans = ArgumentSpan.fromString(spansString, words).toOptionPrinting.get
    } yield PredicateArgumentStructure(pred, argSpans)
    CoNLLSentence(sentenceNum, words, tree, paStructures)
  }
}

case class CoNLLWord(
  index: Int,
  token: String,
  pos: String)
