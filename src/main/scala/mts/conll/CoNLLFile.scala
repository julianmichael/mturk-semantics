package mts.conll

case class CoNLLPath(get: String)

case class CoNLLSentencePath(
  filePath: CoNLLPath,
  sentenceNum: Int
)

case class CoNLLFile(
  id: String,
  sentences: Vector[CoNLLSentence]
)
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
  syntaxTree: SyntaxTree
  // nerSpans: Nothing, // TODO
  // corefSpans: List[CorefSpan] // TODO
)

object CoNLLSentence {
  def readFromLines(sentenceNum: Int, lines: List[String]): CoNLLSentence = {
    val wordArrays = lines.map(_.split("\\s+"))
    val words = wordArrays.map(arr => CoNLLWord(arr(2).toInt, arr(3), arr(4)))
    val treeString = wordArrays.map(arr => arr(5)).mkString
    val tree = SyntaxTree.fromString(treeString, words).get
    CoNLLSentence(sentenceNum, words, tree)
  }
}

case class CoNLLWord(
  index: Int,
  token: String,
  pos: String
  // predicateArgumentStructure: Nothing, // TODO
  // wordSense: Nothing // TODO
)
