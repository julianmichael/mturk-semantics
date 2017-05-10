package nlpdata.datasets.ptb

import nlpdata.structure._

case class PTBFile(sentences: Vector[PTBSentence])

case class PTBSentence(
  sentenceNum: Int,
  words: Vector[Word],
  syntaxTree: SyntaxTree)

case class PTBPath(suffix: String)

case class PTBSentencePath(filePath: PTBPath, sentenceNum: Int)
