package mts.datasets

import scala.util.Try

package object propbank extends PackagePlatformExtensions {
  type Word = ptb.Word
  val Word = ptb.Word
  type SyntaxTree = ptb.SyntaxTree
  type SyntaxTreeNode = ptb.SyntaxTreeNode
  val SyntaxTreeNode = ptb.SyntaxTreeNode
  type SyntaxTreeLeaf = ptb.SyntaxTreeLeaf
  val SyntaxTreeLeaf = ptb.SyntaxTreeLeaf

  implicit class PropBankTextRendering(val tr: mts.language.TextRendering.type) extends AnyVal {
    def renderSentence(sentence: PropBankSentence): String = {
      tr.renderSentence(sentence.words.map(_.token))
    }

    def renderSpan(sentence: PropBankSentence, span: Set[Int]) = {
      tr.renderSentence(sentence.words.map(_.token).zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
    }
  }

  /** Provides parsing of argument spans. */
  object Parsing {

    // TODO import more specifically...
    import scalaz._
    import Scalaz._
    private[this] type SpanState[A] = State[List[Word], A]

    import fastparse.all._
    private[this] val popWord: SpanState[Word] = for {
      words <- State.get
      _ <- State.put(words.tail)
    } yield words.head

    private[this] val labelP: P[String] =
      P(CharIn(('A' to 'Z') ++ ('0' to '9') ++ Seq('-')).rep.!)

    private[this] val wordP: P[SpanState[Word]] =
      P("*").map(_ => popWord)

    private[this] val wordsP: P[SpanState[List[Word]]] =
      P(wordP.rep).map(_.toList.sequence)

    private[this] val spanP: P[SpanState[ArgumentSpan]] =
      P("(" ~ labelP ~ wordsP ~ ")").map {
        case (label, wordsState) => for {
          words <- wordsState
        } yield ArgumentSpan(label, words)
      }

    private[this] val spanAndWordsP: P[SpanState[ArgumentSpan]] =
      P(wordsP ~ spanP).map {
        case (wordsState, spanState) => for {
          _ <- wordsState
          span <- spanState
        } yield span
      }

    private[this] val allSpansP: P[SpanState[List[ArgumentSpan]]] =
      P(spanAndWordsP.rep ~ wordsP).map {
        case (spanStates, finalWords) => for {
          spans <- spanStates.toList.sequence
          // TODO return an informative error if finalWords aren't all used up by the end
          _ <- finalWords
        } yield spans
      }

    def readArgumentSpans(s: String, words: List[Word]): List[ArgumentSpan] =
      allSpansP.parse(s).get.value.eval(words)

    def readSentence(sentencePath: PropBankSentencePath, lines: List[String]): PropBankSentence = {
      val lineArrays = lines.map(_.split("\\s+"))
      val words = lineArrays.map(arr => Word(arr(2).toInt, arr(4), arr(3)))
      val treeString = lineArrays.map(arr => arr(5)).mkString
      val tree = conll.Parsing.readSyntaxTree(treeString, words)
      val predicates = for {
        (arr, index) <- lineArrays.zipWithIndex
        predicateLemma = arr(6)
        if !predicateLemma.equals("-")
        framesetIdString = arr(7)
        head = words(index)
      } yield Predicate(head, predicateLemma, framesetIdString)
      val paStructures = for {
        (pred, num) <- predicates.zipWithIndex // num of predicate tells us which col the args are in
        spansString = lineArrays.map(arr => arr(8 + num)).mkString
        argSpans = readArgumentSpans(spansString, words)
      } yield PredicateArgumentStructure(pred, argSpans)
      PropBankSentence(sentencePath, words, tree, paStructures)
    }

    def readFile(path: PropBankPath, lines: Iterator[String]): PropBankFile = {
      val (sentences, _, _) = lines
        .foldLeft((List.empty[PropBankSentence], List.empty[String], 0)) {
        case ((prevSentences, curLines, sentenceNum), line) =>
          if(line.trim.isEmpty) {
            (readSentence(PropBankSentencePath(path, sentenceNum), curLines.reverse) :: prevSentences, Nil, sentenceNum + 1)
          } else {
            (prevSentences, line :: curLines, sentenceNum)
          }
      }
      PropBankFile(path, sentences.toVector.reverse)
    }
  }

}
