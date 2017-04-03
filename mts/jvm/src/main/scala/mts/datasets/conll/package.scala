package mts.datasets.conll

import mts.util._

import scala.util.{Try, Success, Failure}
import scala.collection.mutable
import scala.concurrent.duration._

import scala.language.implicitConversions
import scala.language.postfixOps

import java.nio.file.{Paths, Path, Files}

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
trait PackagePlatformExtensions {

  import mts.datasets.ptb
  def ptbToCoNLLPath(ptbPath: ptb.PTBPath): Option[CoNLLPath] = {
    val wsjPathRegex = """(.*)\.MRG""".r
    val wsjPathRegex(suffix) = ptbPath.suffix
    val conllPath = CoNLLPath(s"nw/wsj/${suffix.toLowerCase}.v4_gold_conll")
    FileManager.getCoNLLFile(conllPath).toOption.map(_ => conllPath)
  }
  def ptbToCoNLLSentencePath(ptbPath: ptb.PTBSentencePath): Option[CoNLLSentencePath] =
    ptbToCoNLLPath(ptbPath.filePath).map(
      CoNLLSentencePath(_, ptbPath.sentenceNum)
    )

  /** Implicitly converts vanilla FileManager to the CoNLL one,
    * so you don't have to remember how to prefix the term "FileManager" to call these methods.
    */
  implicit def fileManagerToCoNLL(fm: FileManager.type): CoNLLFileManager.type = CoNLLFileManager

  object CoNLLFileManager {

    import com.softwaremill.macmemo.memoize
    import com.softwaremill.macmemo.MemoCacheBuilder
    implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

    private[this] val conllTrainPath = Paths.get("conll-2012/v4/data/train/data/english/annotations")
    private[this] val conllDevPath = Paths.get("conll-2012/v4/data/development/data/english/annotations")

    @memoize(maxSize = 200, expiresAfter = 1 hour)
    private[this] def getCoNLLFileUnsafe(path: CoNLLPath): CoNLLFile = {
      def getFromRoot(rootPath: Path) = {
        val fullPath = rootPath.resolve(path.get)
        for {
          lines <- FileManager.loadResource(fullPath)
          file = Parsing.readFile(lines)
        } yield file
      }
      getFromRoot(conllDevPath).tried.recoverWith {
        case e: java.nio.file.NoSuchFileException => getFromRoot(conllTrainPath).tried
        case e => e.printStackTrace; Failure(e)
      }.get
    }

    def getCoNLLFile(path: CoNLLPath): Try[CoNLLFile] =
      Try(getCoNLLFileUnsafe(path))

    def getCoNLLSentence(path: CoNLLSentencePath): Try[CoNLLSentence] = for {
      file <- getCoNLLFile(path.filePath)
    } yield file.sentences(path.sentenceNum)
  }
}
