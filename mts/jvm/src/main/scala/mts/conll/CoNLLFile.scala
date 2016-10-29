package mts.conll

import mts.util._
import scala.util.Try

/** Represents a single CoNLL annotation file.
  *
  * @param id the unique ID of the file, present on its first line
  * @param sentences all of the sentences in the annotation file
  */
case class CoNLLFile(
  id: String,
  sentences: Vector[CoNLLSentence]
)

/** Provides parsing of CoNLL files.
  *
  * See http://conll.cemantix.org/2012/data.html for the data format.
  */
object CoNLLFile {

  /** Reads a CoNLLFile from an iterator over lines.
    *
    * Assumes that the given lines are taken directly from a CoNLL file.
    * Behavior is undefined if not.
    *
    * @param lines the lines of a CoNLL file
    */
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

/** Represents a path to a CoNLL file.
  * Contains only the suffix of the path after `annotations`,
  * ignoring the absolute path to the CoNLL 2012 data.
  *
  * @param get the file path suffix
  */
case class CoNLLPath(get: String)
