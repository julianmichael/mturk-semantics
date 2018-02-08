package turksem.util

import nlpdata.structure.AlignedToken

object Tokenizer {
  /** Tokenizes an English string. */
  def tokenize(s: String): Vector[String] = {
    import java.io.StringReader
    import edu.stanford.nlp.process.PTBTokenizer
    import edu.stanford.nlp.process.WordTokenFactory
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toVector.map(_.word)
  }

  def tokenizeAligned(s: String): Vector[AlignedToken] = {
    import java.io.StringReader
    import edu.stanford.nlp.process.PTBTokenizer
    import edu.stanford.nlp.process.CoreLabelTokenFactory
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new CoreLabelTokenFactory(), "invertible=true")
      .tokenize.asScala.toVector.map(coreLabel =>
      AlignedToken(
        token = coreLabel.word,
        originalText = coreLabel.originalText,
        whitespaceBefore = coreLabel.before,
        whitespaceAfter = coreLabel.after)
    )
  }
}
