package mts.conll

import mts.util._
import scala.util.Try

/** Represents a single predicate--argument structure.
  *
  * A CoNLL sentence contains a list of these;
  * the words in the head and argument spans will be the same words
  * that appear in the CoNLL sentence itself.
  *
  * @param pred the predicate of the PAS, including the head word
  * @param arguments the argument spans
  */
case class PredicateArgumentStructure(
  pred: Predicate,
  arguments: List[ArgumentSpan])

/** Represents the predicate of a predicate--argument structure.
  *
  * @param head the head word
  * @param predicateLemma the predicate lemma as written in the CoNLL data
  * @param framesetId the PropBank frameset ID as written in the CoNLL data
  */
case class Predicate(
  head: CoNLLWord,
  predicateLemma: String,
  framesetId: Int)

/** Represents an argument span in a predicate--argument structure.
  *
  * @param label the dependency label for this span
  * @param words the words in the span, assumed a contiguous subsequence of the sentence
  */
case class ArgumentSpan(
  label: String,
  words: List[CoNLLWord]
) {

  /** The beginning index of the argument span.
    * -1 in case of an empty span, though I'm not sure when that will happen. (hopefully never)
    */
  val beginIndex = words.headOption.map(_.index).getOrElse(-1)

  /** The beginning index of the argument span.
    * -1 in case of an empty span, though I'm not sure when that will happen. (hopefully never)
    */
  val endIndex = words.lastOption.map(_.index + 1).getOrElse(-1)
}

/** Provides parsing of argument spans. */
object ArgumentSpan {

  // TODO import more specifically...
  import scalaz._
  import Scalaz._
  private[this] type SpanState[A] = State[List[CoNLLWord], A]

  import fastparse.all._
  private[this] val popWord: SpanState[CoNLLWord] = for {
    words <- State.get
    _ <- State.put(words.tail)
  } yield words.head

  private[this] val labelP: P[String] =
    P(CharIn(('A' to 'Z') ++ ('0' to '9') ++ Seq('-')).rep.!)

  private[this] val wordP: P[SpanState[CoNLLWord]] =
    P("*").map(_ => popWord)

  private[this] val wordsP: P[SpanState[List[CoNLLWord]]] =
    P(wordP.rep).map(_.toList.sequence)

  private[this] val spanP: P[SpanState[ArgumentSpan]] =
    P("(" ~ labelP ~ wordsP ~ ")").map {
      case (label, wordsState) => for {
        words <- wordsState
      } yield ArgumentSpan(label, words)
    }

  private[this] val spanAndWordsP: P[SpanState[ArgumentSpan]] =
    P(wordsP ~ spanP).map {
      case (wordsState, spanState) => for {
        _ <- wordsState
        span <- spanState
      } yield span
    }

  private[this] val allSpansP: P[SpanState[List[ArgumentSpan]]] =
    P(spanAndWordsP.rep ~ wordsP).map {
      case (spanStates, finalWords) => for {
        spans <- spanStates.toList.sequence
        // TODO return an informative error if finalWords aren't all used up by the end
        _ <- finalWords
      } yield spans
    }

  /** Parses a list of argument spans from their column representation in the CoNLL data.
    *
    * Also requires a list of all of the words in the sentence.
    * Assumes that the data is formatted correctly.
    *
    * @param s the flattened CoNLL column representation of a list of argument spans
    * @param words all of the words of the sentence containing the spans
    * @return the list of ArgumentSpan denoted by the data
    */
  def fromString(s: String, words: List[CoNLLWord]): List[ArgumentSpan] =
    allSpansP.parse(s).get.value.eval(words)
}
