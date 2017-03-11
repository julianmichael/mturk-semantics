package mts.datasets.propbank

case class PropBankFile(
  path: PropBankPath,
  sentences: Vector[PropBankSentence])

case class PropBankSentence(
  path: PropBankSentencePath,
  words: List[Word],
  syntaxTree: SyntaxTree,
  predicateArgumentStructures: List[PredicateArgumentStructure]) {
  def sentenceNum = path.sentenceNum
}

case class PropBankPath(get: String)

case class PropBankSentencePath(filePath: PropBankPath, sentenceNum: Int) {
  override def toString = s"${filePath.get}:$sentenceNum"
}
