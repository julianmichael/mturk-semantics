package qamr

case class QAPairId[SID](
  sentenceId: SID,
  keywords: List[Int],
  workerId: String,
  qaIndex: Int)
