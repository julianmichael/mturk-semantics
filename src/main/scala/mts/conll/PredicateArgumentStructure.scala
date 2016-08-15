package mts.conll

import mts.util._
import scala.util.Try

case class PredicateArgumentStructure(
  pred: Predicate,
  arguments: List[ArgumentSpan])

case class Predicate(
  head: CoNLLWord,
  predicateLemma: String,
  framesetId: Int)

case class ArgumentSpan(
  label: String,
  words: List[CoNLLWord]
) {
  // empty span I guess could mean implicit/omitted argument? won't happen in CoNLL data
  // probably better than assuming nonempty spans or whatever.
  val beginIndex = words.headOption.map(_.index).getOrElse(-1)
  val endIndex = words.lastOption.map(_.index).getOrElse(-1)
}
object ArgumentSpan {
  // TODO import more specifically...
  import scalaz._
  import Scalaz._
  type SpanState[A] = State[List[CoNLLWord], A]

  import fastparse.all._
  val popWord: SpanState[CoNLLWord] = for {
    words <- State.get
    _ <- State.put(words.tail)
  } yield words.head
  val labelP: P[String] =
    P(CharIn(('A' to 'Z') ++ ('0' to '9') ++ Seq('-')).rep.!)
  val wordP: P[SpanState[CoNLLWord]] =
    P("*").map(_ => popWord)
  val wordsP: P[SpanState[List[CoNLLWord]]] =
    P(wordP.rep).map(_.toList.sequence)
  val spanP: P[SpanState[ArgumentSpan]] =
    P("(" ~ labelP ~ wordsP ~ ")").map {
      case (label, wordsState) => for {
        words <- wordsState
      } yield ArgumentSpan(label, words)
    }
  val spanAndWordsP: P[SpanState[ArgumentSpan]] =
    P(wordsP ~ spanP).map {
      case (wordsState, spanState) => for {
        _ <- wordsState
        span <- spanState
      } yield span
    }
  val allSpansP: P[SpanState[List[ArgumentSpan]]] =
    P(spanAndWordsP.rep ~ wordsP).map {
      case (spanStates, finalWords) => for {
        spans <- spanStates.toList.sequence
        _ <- finalWords
      } yield spans
    }

  def fromString(s: String, words: List[CoNLLWord]): Try[List[ArgumentSpan]] = Try {
    allSpansP.parse(s).get.value.eval(words)
  }
}
