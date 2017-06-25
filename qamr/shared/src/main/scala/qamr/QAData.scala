package qamr

import qamr.util._

class QAData[SID](
  val all: List[SourcedQA[SID]],
  val idToQA: Map[QAPairId[SID], SourcedQA[SID]],
  val sentenceToQAs: Map[SID, List[SourcedQA[SID]]]
) {
  // lazy val all = allUnfiltered.filter(_.isGood)
  // lazy val idToQA = idToQAUnfiltered.filter(x => x._2.isGood)
  // lazy val sentenceToQAs = sentenceToQAsUnfiltered.flatMap { case (id, qas) =>
  //   val newQAs = qas.filter(_.isGood)
  //   Some(id -> newQAs).filter(const(newQAs.nonEmpty))
  // }

  def this(_all: List[SourcedQA[SID]]) = this(
    _all,
    _all.map(sqa => sqa.id -> sqa).toMap,
    _all.groupBy(_.id.sentenceId))

  def filterBySentence(p: SID => Boolean) = new QAData(
    all.filter(sqa => p(sqa.id.sentenceId)),
    idToQA.filter(x => p(x._1.sentenceId)),
    sentenceToQAs.filter(x => p(x._1)))

  def filterByQA(p: SourcedQA[SID] => Boolean) = new QAData(
    all.filter(p),
    idToQA.filter(x => p(x._2)),
    sentenceToQAs.flatMap { case (id, qas) =>
      val newQAs = qas.filter(p)
      Some(id -> newQAs).filter(const(newQAs.nonEmpty))
    })
}

object QAData {
  def apply[SID](sqas: List[SourcedQA[SID]]): QAData[SID] = {
    val idToQA = sqas.map(sqa => sqa.id -> sqa).toMap
    val sentenceToQAs = sqas.groupBy(_.id.sentenceId)
    new QAData(sqas, idToQA, sentenceToQAs)
  }
}
