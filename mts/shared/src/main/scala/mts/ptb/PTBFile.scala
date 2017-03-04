package mts.ptb

import mts.util._
import scala.util.Try

case class PTBPath(suffix: String)

/** Represents a single PTB annotation file.
  *
  * @param sentences all of the sentences in the annotation file
  */
case class PTBFile(sentences: Vector[PTBSentence])

/** Provides parsing of PTB files.
  */
object PTBFile {

  /** Reads a PTBFile from an iterator over lines.
    *
    * Assumes that the given lines are taken directly from a PTB file.
    * Behavior is undefined if not.
    *
    * @param lines the lines of a PTB file
    */
  def readFromLines(lines: Iterator[String]): PTBFile = {
    val (sentences, lastChunk, lastIndex) = lines
      .foldLeft((List.empty[PTBSentence], List.empty[String], 0)) {
      case ((prevSentences, curLines, sentenceNum), line) =>
        if(line.isEmpty) {
          (prevSentences, curLines, sentenceNum)
        } else if(!line.startsWith(" ") && !curLines.isEmpty) {
          val tree = SyntaxTree.fromString(curLines.reverse.map(_.dropWhile(_ == ' ')).mkString)
          val sentence = PTBSentence(sentenceNum, tree.words, tree)
          (sentence :: prevSentences, line :: Nil, sentenceNum + 1)
        } else {
          (prevSentences, line :: curLines, sentenceNum)
        }
    }
    val lastSentence = {
      val tree = SyntaxTree.fromString(lastChunk.reverse.map(_.dropWhile(_ == ' ')).mkString)
      PTBSentence(lastIndex, tree.words, tree)
    }
    PTBFile((lastSentence :: sentences).toVector.reverse)
  }
}
