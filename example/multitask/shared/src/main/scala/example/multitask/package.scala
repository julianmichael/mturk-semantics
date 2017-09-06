package example

import nlpdata.datasets.ptb3._

import cats.Order
import cats.implicits._

package object multitask {
  type SentenceId = PTB3SentencePath

  implicit object sentenceIdOrder extends Order[SentenceId] {
    override def compare(x: SentenceId, y: SentenceId): Int = (x.filepath, y.filepath) match {
      case (WSJPath(_, _), BrownPath(_, _)) => -1
      case (BrownPath(_, _), WSJPath(_, _)) => 1
      case (WSJPath(xSection, xNum), WSJPath(ySection, yNum)) =>
        Option(xSection - ySection).filter(_ != 0).getOrElse(
          Option(xNum - yNum).filter(_ != 0).getOrElse(
            x.sentenceNum - y.sentenceNum))

      case (BrownPath(xDomain, xNum), BrownPath(yDomain, yNum)) =>
        Option(xDomain.compare(yDomain)).filter(_ != 0).getOrElse(
          Option(xNum - yNum).filter(_ != 0).getOrElse(
            x.sentenceNum - y.sentenceNum))
    }
  }
  implicit val sentenceIdOrdering = sentenceIdOrder.toOrdering
}
