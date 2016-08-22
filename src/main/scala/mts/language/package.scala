package mts

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import mts.util._

package object language {

  private[this] val stopwordFilePath = FileManager.resourcePath
    .resolve("english.stop.txt")
  private[this] val wiktionaryFilepath = FileManager.resourcePath
    .resolve("wiktionary")
    .resolve("en_verb_inflections.txt");

  lazy val stopwords: Set[String] = {
    import scala.collection.JavaConverters._
    val fileStream = Files.lines(stopwordFilePath)
    val result = fileStream.iterator.asScala.toSet ++ Set("hm", "uh", "um")
    fileStream.close()
    result
  }

  def getInflectionsForTokens(tokens: Iterator[String]): Inflections = {
    val wordDict = new CountDictionary()
    tokens.foreach(wordDict.addString)
    val inflDict = new VerbInflectionDictionary(wordDict)
    inflDict.loadDictionaryFromFile(wiktionaryFilepath.toString)
    new Inflections(inflDict)
  }

  def tokenize(s: String): List[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toList.map(_.word)
  }

}
