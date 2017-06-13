package mts

import mts.util._

class QAData[SID](
  val allUnfiltered: List[SourcedQA[SID]],
  val idToQAUnfiltered: Map[QAPairId[SID], SourcedQA[SID]],
  val sentenceToQAsUnfiltered: Map[SID, List[SourcedQA[SID]]]
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
    allUnfiltered.filter(sqa => p(sqa.id.sentenceId)),
    idToQAUnfiltered.filter(x => p(x._1.sentenceId)),
    sentenceToQAsUnfiltered.filter(x => p(x._1)))

  def filterByQA(p: SourcedQA[SID] => Boolean) = new QAData(
    allUnfiltered.filter(p),
    idToQAUnfiltered.filter(x => p(x._2)),
    sentenceToQAsUnfiltered.flatMap { case (id, qas) =>
      val newQAs = qas.filter(p)
      Some(id -> newQAs).filter(const(newQAs.nonEmpty))
    })
}
