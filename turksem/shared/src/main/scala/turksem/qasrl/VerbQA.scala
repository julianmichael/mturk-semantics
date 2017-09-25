package turksem.qasrl

case class VerbQA(
  verbIndex: Int,
  question: String, // should be guaranteed to adhere to QA-SRL format
  answers: List[Set[Int]] // should not overlap
)
