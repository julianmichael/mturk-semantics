package mts

import mts.util._

import scala.util.{Try, Success, Failure}
import scala.collection.mutable

/** Provides logic for interfacing with the CoNLL-2012 data.
  * Includes data types and methods for reading instances from the data files.
  *
  * The main entry points are the FileManager extension methods,
  * used to read CoNLL files and sentences from paths.
  *
  * As of now, I don't have a programmatic store of all of the paths.
  * If you wish to load a CoNLL file, you will have to create the path yourself,
  * or find paths already in the code (for example, the sample paths in
  * [[mts.experiments]]).
  *
  * The package object contains extension methods for conveniently rendering text
  * from CoNLL data types and reading CoNLL data from the CoNLL-2012 files.
  */
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
        val fileStream = Files.lines(fullPath)
        val lines = fileStream.iterator.asScala
        val file = CoNLLFile.readFromLines(lines)
        fileStream.close()
        conllCache.put(path, file)
        file
      }
    }

    def getCoNLLSentence(path: CoNLLSentencePath): Try[CoNLLSentence] = for {
      file <- getCoNLLFile(path.filePath)
    } yield file.sentences(path.sentenceNum)
  }
}
