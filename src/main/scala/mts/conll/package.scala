package mts

import mts.util._

import scala.util.{Try, Success, Failure}
import scala.collection.mutable

package object conll {

  implicit class CoNLLTextRendering(val tr: TextRendering.type) extends AnyVal {
    def renderSentence(sentence: CoNLLSentence) = {
      tr.renderSentence(sentence.words.map(_.token))
    }
  }

  // TODO bound the cache's memory use / number of files
  private[this] val conllCache = mutable.Map.empty[CoNLLPath, CoNLLFile]
  import java.nio.file.{Paths, Path, Files}
  private[this] val conllRootPath = FileManager.resourcePath.resolve(Paths.get("conll-2012"))
  private[this] val conllAnnotationPath = conllRootPath.resolve("v4/data/development/data/english/annotations")

  implicit class CoNLLFileManager(val fm: FileManager.type) extends AnyVal {

    def getCoNLLFile(path: CoNLLPath): Try[CoNLLFile] = Try {
      if(conllCache.contains(path)) {
        conllCache(path)
      } else {
        val fullPath = conllAnnotationPath.resolve(path.get)
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
