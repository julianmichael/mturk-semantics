package mts.datasets.conll

import mts.util._

/** Represents a single CoNLL annotation file.
  *
  * @param id the unique ID of the file, present on its first line
  * @param sentences all of the sentences in the annotation file
  */
case class CoNLLFile(
  id: String,
  sentences: Vector[CoNLLSentence]
)

/** Represents an annotated sentence from the CoNLL data.
  *
  * As of now, we're lazy-loading the members of this class,
  * only implementing them as we need them.
  * I believe coref spans are already implemented in the coref annotation project;
  * if necessary ask me (Julian) and I can put them in.
  */
case class CoNLLSentence(
  // partNum: Int,
  sentenceNum: Int,
  words: List[Word],
  syntaxTree: SyntaxTree,
  predicateArgumentStructures: List[PredicateArgumentStructure]
    // nerSpans: Nothing, // TODO
    // corefSpans: List[CorefSpan] // TODO
)

/** Represents a path to a CoNLL file.
  * Contains only the suffix of the path after `annotations`,
  * ignoring the absolute path to the CoNLL 2012 data.
  *
  * @param get the file path suffix
  */
case class CoNLLPath(get: String)

/** Represents a unique index to a CoNLL sentence.
  *
  * This can be used to easily serialize a sentence without worrying about the data definition changing.
  * The FileManager extension methods for the conll package include one to retrieve a sentence directly
  * from such a path.
  *
  * @param filePath the path to the CoNLL file containing this sentence
  * @param sentenceNum the index of this sentence in the document
  */
case class CoNLLSentencePath(
  filePath: CoNLLPath,
  sentenceNum: Int
) {
  override def toString = s"${filePath.get}:$sentenceNum"
}
