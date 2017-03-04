package mts.ptb

case class PTBSentencePath(filePath: PTBPath, sentenceNum: Int)

case class PTBSentence(
  sentenceNum: Int,
  words: Vector[Word],
  syntaxTree: SyntaxTree)
