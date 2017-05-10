package nlpdata.datasets.qasrl

import nlpdata.structure._
import nlpdata.datasets.ptb._

// for now, just stringify the questions... can make better later

// case class QASRLLabel(
//   wh: String,
// )

case class QASRLSentence(
  ptbPath: PTBSentencePath,
  tokensOriginal: List[String],
  predicateArgumentStructuresOriginal: List[PredicateArgumentStructure]
) {
  val wordsOriginal = tokensOriginal.zipWithIndex.map { case (token, index) =>
    Word(token = token, index = index, pos = "")
  }
  // XXX copied from propbank code; could factor into single place
  val (tokens, predicateArgumentStructures) = {
    val wordMapping = wordsOriginal.foldRight((List.empty[List[Word]], false)) {
      case (w, (ws, hasHyphen)) => ws match {
        case prevGroup :: rest if hasHyphen => ((w :: prevGroup) :: rest, false)
        case prevGroup :: rest if w.token == "-" => ((w :: prevGroup) :: rest, true)
        case prevGroups => (List(w) :: prevGroups, w.token == "-")
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
