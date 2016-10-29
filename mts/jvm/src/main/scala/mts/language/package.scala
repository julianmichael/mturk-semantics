package mts

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import mts.util._

/** Provides classes and methods for working with natural language strings.
  *
  * This includes resources like a set of stopwords (currently not being used),
  * a tokenizer (provided by Stanford CoreNLP),
  * and inflections (drawn from Wiktionary).
  */
package object language {

  private[this] val stopwordFilePath = Paths.get("english.stop.txt")
  private[this] val wiktionaryFilepath = Paths.get("wiktionary").resolve("en_verb_inflections.txt")

  /** Stopword set from a local file.
    *
    * Not sure where the file came from, but I found it in my old repo
    * from Dan Garrette's undergrad NLP class.
    */
  lazy val stopwords: Set[String] = {
    val setResource = for {
      lines <- FileManager.loadResource(stopwordFilePath)
    } yield lines.toSet ++ Set("hm", "uh", "um")
    setResource.tried.get
  }

  /** Constructs an Inflections object containing all known inflections
    * for a given set of words.
    *
    * @param tokens an iterator over all words we might want inflections for
    */
  def getInflectionsForTokens(tokens: Iterator[String]): Inflections = {
    val wordDict = new CountDictionary()
    tokens.foreach(wordDict.addString)
    val inflDict = new VerbInflectionDictionary(wordDict)
    inflDict.loadDictionaryFromFile(wiktionaryFilepath.toString)
    new Inflections(inflDict)
  }

  /** Tokenizes an English string. */
  def tokenize(s: String): List[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toList.map(_.word)
  }

}
