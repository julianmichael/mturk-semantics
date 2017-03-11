package mts.datasets

import ptb._

package object nombank extends PackagePlatformExtensions {
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

  case class SpanIndicator(
    leftIndex: Int,
    height: Int) {
    def upOne = copy(height = this.height + 1)
  }

  // for now, ignoring different kinds of linking... TODO do this perhaps
  case class LinkedSpanIndicator(
    label: String,
    spanIndicators: List[SpanIndicator])

  case class NomBankEntry(
    ptbSentencePath: PTBSentencePath,
    headIndex: Int,
    predLemma: String,
    framesetId: Int,
    argSpanIndicators: List[LinkedSpanIndicator])

  // expect PTB trees, i.e., with -NONE-s and such
  // also does not do anything interesting to linked spans; puts them together under one label
  def getPredicateArgumentStructure(entry: NomBankEntry, refTree: SyntaxTree) = entry match {
    case NomBankEntry(ptbSentencePath, headIndex, predLemma, framesetId, argSpanIndicators) =>
      val words = refTree.words
      val head = words(headIndex)
      val predicate = Predicate(head, predLemma, framesetId)
      val argumentSpans = argSpanIndicators.map {
        case LinkedSpanIndicator(label, spanIndicators) =>
          val words = refTree.foldUnlabeled(w => (List.empty[Word], SpanIndicator(w.index, 0), List(w))) { children =>
            val oldWords = children.flatMap(_._1)
            val newWords = children.filter(t => spanIndicators.contains(t._2)).flatMap(_._3)
            val newIndicator = children.head._2.upOne
            val allWords = children.flatMap(_._3)
            (oldWords ++ newWords, newIndicator, allWords)
          }._1
          ArgumentSpan(label, words)
      }
      PredicateArgumentStructure(predicate, argumentSpans)
  }

  /** Provides parsing of argument spans. */
  object Parsing {
    val PTBString = """wsj/(.*)""".r
    object IntMatch {
      val IntMatchRegex = "(\\d+)".r
      def unapply(s: String): Option[Int] = s match {
        case IntMatchRegex(num) => Some(num.toInt)
        case _ => None
      }
    }

    def readEntry(line: String) = line.split(" ").toList match {
      case PTBString(pathSuffix) :: IntMatch(sentenceNum) :: IntMatch(headIndex) :: predLemma :: IntMatch(framesetId) :: argStrings =>
        val arguments = argStrings.map { argStr =>
          val (spansStr, label) = argStr.span(_ != '-')
          val spanStrings = spansStr.split("[\\*;,]").toList
          val spans = spanStrings.map(
            _.split(":").toList match {
              case IntMatch(leftIndex) :: IntMatch(height) :: Nil => SpanIndicator(leftIndex, height)
            }
          )
          LinkedSpanIndicator(label.tail, spans)
        }
        NomBankEntry(
          PTBSentencePath(PTBPath(pathSuffix.toUpperCase), sentenceNum),
          headIndex, predLemma, framesetId,
          arguments)
    }
  }
}
