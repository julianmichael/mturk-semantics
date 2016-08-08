package mts

import mts.util._

import scala.util.{Try, Success, Failure}
import scala.collection.mutable

package object conll {
  val annotationFilepaths = List(
    "bn/abc/00/abc_0010.v4_gold_conll",
    "mz/sinorama/10/ectb_1010.v4_gold_conll",
    "bc/msnbc/00/msnbc_0000.v4_gold_conll",
    "nw/wsj/24/wsj_2400.v4_gold_conll",
    "nw/xinhua/00/chtb_0010.v4_gold_conll",
    "pt/nt/40/nt_4010.v4_gold_conll",
    // "tc/ch/00/ch_0010.v4_gold_conll",
    "wb/eng/00/eng_0000.v4_gold_conll"
  ).map(CoNLLPath.apply)

  def conllSentences(): Iterator[(CoNLLSentencePath, String)] = for {
    path <- annotationFilepaths.iterator
    file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
    sentence <- file.sentences
    sentenceString = TextRendering.renderSentence(sentence)
  } yield (CoNLLSentencePath(path, sentence.sentenceNum), sentenceString)

  implicit class CoNLLTextRendering(val tr: TextRendering.type) extends AnyVal {
    def renderSentence(sentence: CoNLLSentence) = {
      tr.renderSentence(sentence.words.map(_.token))
    }
  }

  // TODO bound the cache's memory use / number of files
  private[this] val conllCache = mutable.Map.empty[CoNLLPath, CoNLLFile]
  import java.nio.file.{Paths, Path, Files}
  private[this] val CoNLLRootPath = Paths.get("conll-2012")
  private[this] val CoNLLAnnotationPath = CoNLLRootPath.resolve("v4/data/development/data/english/annotations")

  implicit class CoNLLFileManager(val fm: FileManager.type) extends AnyVal {

    def getCoNLLFile(path: CoNLLPath): Try[CoNLLFile] = Try {
      if(conllCache.contains(path)) {
        conllCache(path)
      } else {
        val fullPath = CoNLLAnnotationPath.resolve(path.get)
        import scala.collection.JavaConverters._
        val lines = Files.lines(fullPath).iterator.asScala
        val file = CoNLLFile.readFromLines(lines)
        conllCache.put(path, file)
        file
      }
    }

    def getCoNLLSentence(path: CoNLLSentencePath): Try[CoNLLSentence] = for {
      file <- getCoNLLFile(path.filePath)
    } yield file.sentences(path.sentenceNum)
  }
}
