package nlpdata.structure

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
  head: Word,
  predicateLemma: String,
  framesetId: String)

/** Represents an argument span in a predicate--argument structure.
  *
  * @param label the dependency label for this span
  * @param words the words in the span, assumed a contiguous subsequence of the sentence
  */
case class ArgumentSpan(
  label: String,
  words: List[Word]
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
