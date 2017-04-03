package mts.datasets

import mts.language.TextRendering
import scala.util.Try

package object conll extends PackagePlatformExtensions {
  type Word = ptb.Word
  val Word = ptb.Word
  type SyntaxTree = ptb.SyntaxTree
  type SyntaxTreeNode = ptb.SyntaxTreeNode
  val SyntaxTreeNode = ptb.SyntaxTreeNode
  type SyntaxTreeLeaf = ptb.SyntaxTreeLeaf
  val SyntaxTreeLeaf = ptb.SyntaxTreeLeaf
  type PredicateArgumentStructure = propbank.PredicateArgumentStructure
  val PredicateArgumentStructure = propbank.PredicateArgumentStructure
  type Predicate = propbank.Predicate
  val Predicate = propbank.Predicate
  type ArgumentSpan = propbank.ArgumentSpan
  val ArgumentSpan = propbank.ArgumentSpan

  implicit class CoNLLTextRendering(val tr: TextRendering.type) extends AnyVal {
    def renderSentence(sentence: CoNLLSentence) = {
      tr.renderSentence(sentence.words.map(_.token))
    }
  }

  object Parsing {
    // TODO import more specifically
    import scalaz._
    import Scalaz._
    private[this] type SentenceState[A] = State[List[Word], A]

    import fastparse.all._
    private[this] val symbolP: P[String] = P(CharIn('A' to 'Z').rep.!)
    private[this] lazy val treeP: P[SentenceState[SyntaxTree]] =
      P("(" ~ symbolP ~ treeP.rep ~ ")").map {
        case (symbol, childrenState) => for {
          children <- childrenState.toList.sequence
        } yield SyntaxTreeNode(symbol, children.toList): SyntaxTree
      } | P("*").map { _ =>
        for {
          words <- State.get
          _ <- State.put(words.tail)
        } yield SyntaxTreeLeaf(words.head)
      }

    /** Parses a SyntaxTree from its flattened column representation in the CoNLL data.
      *
      * Assumes the data is in the correct format. Undefined behavior otherwise.
      *
      * @param s the flattened column representation of the tree
      * @param words the words of the sentence this tree parses
      */
    def readSyntaxTree(s: String, words: List[Word]): SyntaxTree =
      treeP.parse(s).get.value.eval(words)

    /** Reads a CoNLLSentence from a list of lines from a CoNLLFile.
      * This will grow as the number of fields of CoNLLSentence grows.
      *
      * Does not expect empty lines on either end of the list.
      * Assumes the lines are taken from a CoNLL data file,
      * undefined behavior otherwise.
      *
      * @param sentenceNum the index of the sentence in the document
      * @param lines the lines of the file containing the sentence's info
      * @return the CoNLL sentence stored in the data
      */
    def readSentence(sentenceNum: Int, lines: List[String]): CoNLLSentence = {
      val lineArrays = lines.map(_.split("\\s+"))
      val words = lineArrays.map(arr => Word(arr(2).toInt, arr(4), arr(3)))
      val treeString = lineArrays.map(arr => arr(5)).mkString
      val tree = readSyntaxTree(treeString, words)
      val predicates = for {
        (arr, index) <- lineArrays.zipWithIndex
        predicateLemma = arr(6)
        if !predicateLemma.equals("-")
        framesetId = arr(7)
        if !framesetId.equals("-")
        head = words(index)
      } yield Predicate(head, predicateLemma, framesetId)
      val paStructures = for {
        (pred, num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
        spansString = lineArrays.map(arr => arr(11 + num)).mkString
        argSpans = propbank.Parsing.readArgumentSpans(spansString, words)
      } yield PredicateArgumentStructure(pred, argSpans)
      CoNLLSentence(sentenceNum, words, tree, paStructures)
    }

    /** Reads a CoNLLFile from an iterator over lines.
      *
      * Assumes that the given lines are taken directly from a CoNLL file.
      * Behavior is undefined if not.
      * See http://conll.cemantix.org/2012/data.html for the data format.
      *
      * @param lines the lines of a CoNLL file
      */
    def readFile(lines: Iterator[String]): CoNLLFile = {
      val firstLine = lines.next
      val firstLineRegex = """#begin document \((.*)\); part [0-9]+""".r
      val firstLineRegex(id) = firstLine
      val (sentences, _, _) = lines
        .takeWhile(!_.equals("#end document"))
        .foldLeft((List.empty[CoNLLSentence], List.empty[String], 0)) {
        case ((prevSentences, curLines, sentenceNum), line) =>
          if(line.trim.isEmpty) {
            (readSentence(sentenceNum, curLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
          } else {
            (prevSentences, line :: curLines, sentenceNum)
          }
      }
      CoNLLFile(id, sentences.toVector.reverse)
    }
  }
}
