package turksem.iqa

import simulacrum._
import scala.language.implicitConversions

@typeclass trait AdaptiveQuestionGuesser[A] {
  @op("guessForTrigger")
  def guessForTrigger(a: A, state: QuestioningState, triggerWord: InflectionalWord): Option[TemplatedQuestion]

  @op("update")
  def update(a: A, sentence: Vector[InflectionalWord], qas: List[(InflectionalWord, TemplatedQA)]): A

  def empty: A
}
