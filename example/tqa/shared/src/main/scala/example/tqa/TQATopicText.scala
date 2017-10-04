package example.tqa

import nlpdata.structure.AlignedToken

case class TQATopicText(
  lessonName: String,
  topicId: String,
  topicName: String,
  sentences: List[Vector[AlignedToken]])
