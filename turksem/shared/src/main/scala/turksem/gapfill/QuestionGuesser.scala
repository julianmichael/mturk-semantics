package turksem.gapfill

import simulacrum._
import scala.language.implicitConversions

@typeclass trait QuestionGuesser[A] {
  @op("guessForTrigger")
  def guessForTrigger(a: A, state: QuestioningState, triggerGroupIndex: Int): Option[InstantiatedQuestion]
}
