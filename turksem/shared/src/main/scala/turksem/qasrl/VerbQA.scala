package turksem.qasrl

import turksem.util.ContiguousSpan

case class VerbQA(
  verbIndex: Int,
  question: String, // should be guaranteed to adhere to QA-SRL format
  answers: List[ContiguousSpan] // should not overlap
)
