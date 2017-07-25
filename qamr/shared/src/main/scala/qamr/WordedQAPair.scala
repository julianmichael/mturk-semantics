package qamr

/** A QA pair written by a turker on the generation task.
  * includes the index of the word that they were required to include
  * in the question or answer.
  */
case class WordedQAPair(wordIndex: Int, question: String, answer: Set[Int])
