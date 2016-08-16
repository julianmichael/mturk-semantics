package mts

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import mts.util._

package object language {

  private[this] val stopwordFilePath = FileManager.resourcePath.resolve("english.stop.txt")

  lazy val stopwords: Set[String] = {
    import scala.collection.JavaConverters._
    Files.lines(stopwordFilePath).iterator.asScala.toSet ++ Set("hm", "uh", "um")
  }

  def tokenize(s: String): List[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toList.map(_.word)
  }

}
