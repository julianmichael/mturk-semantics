package mts

// for stable references to QA pairs in manual analysis records
case class QAPairId[SID](
  sentenceId: SID,
  keywords: List[Int],
  workerId: String,
  assignmentId: String,
  qaIndex: Int)
