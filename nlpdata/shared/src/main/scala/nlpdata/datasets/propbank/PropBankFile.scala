package nlpdata.datasets.propbank

import nlpdata.structure._

case class PropBankFile(
  path: PropBankPath,
  sentences: Vector[PropBankSentence])

case class PropBankSentence(
  path: PropBankSentencePath,
  wordsOriginal: List[Word],
  syntaxTreeOriginal: SyntaxTree,
  predicateArgumentStructuresOriginal: List[PredicateArgumentStructure]) {
  def sentenceNum = path.sentenceNum

  val (words, predicateArgumentStructures) = {
    val wordMapping = wordsOriginal.foldRight((List.empty[List[Word]], false)) {
      case (w, (ws, hasHyphen)) => ws match {
        case prevGroup :: rest if hasHyphen => ((w :: prevGroup) :: rest, false)
        case prevGroup :: rest if w.pos == "HYPH" => ((w :: prevGroup) :: rest, true)
        case prevGroups => (List(w) :: prevGroups, w.pos == "HYPH")
      }
    }._1.zipWithIndex.flatMap {
      case (group, newIndex) =>
        val newToken = group.map(_.token).mkString
        val newPos = group.last.pos
        val newWord = Word(token = newToken, pos = newPos, index = newIndex)
        group.map(_ -> newWord)
    }.toMap
    val newWords = wordsOriginal.map(wordMapping).toSet.toList.sortBy((w: Word) => w.index)
    val newPAS = predicateArgumentStructuresOriginal.map {
      case PredicateArgumentStructure(pred, args) =>
        PredicateArgumentStructure(
          pred.copy(head = wordMapping(pred.head)),
          args.map { case ArgumentSpan(label, span) =>
            ArgumentSpan(label, span.map(wordMapping).toSet.toList.sortBy((w: Word) => w.index))
          })
    }
    (newWords, newPAS)
  }
}

case class PropBankPath(get: String)

case class PropBankSentencePath(filePath: PropBankPath, sentenceNum: Int) {
  override def toString = s"${filePath.get}:$sentenceNum"
}
