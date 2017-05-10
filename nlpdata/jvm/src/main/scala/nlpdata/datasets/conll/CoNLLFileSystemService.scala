package nlpdata.datasets.conll

import cats.Monad
import cats.implicits._

import nlpdata.util._
import nlpdata.datasets.ptb._

import scala.util.{Try, Success, Failure}
import java.nio.file.{Paths, Path, Files}

class CoNLLFileSystemService(location: Path) extends CoNLLService[Try] {
  override implicit val monad: Monad[Try] = implicitly[Monad[Try]]

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  private[this] implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder
  import scala.concurrent.duration._

  private[this] val conllTrainPath = Paths.get("conll-2012/v4/data/train/data/english/annotations")
  private[this] val conllDevPath = Paths.get("conll-2012/v4/data/development/data/english/annotations")

  @memoize(maxSize = 200, expiresAfter = 1.hour)
  private[this] def getFileUnsafe(path: CoNLLPath): CoNLLFile = {
    def getFromRoot(rootPath: Path) = {
      val fullPath = rootPath.resolve(path.get)
      loadFile(fullPath) map Parsing.readFile
    }
    getFromRoot(conllDevPath).tried.recoverWith {
      case e: java.nio.file.NoSuchFileException => getFromRoot(conllTrainPath).tried
      case e => e.printStackTrace; Failure(e)
    }.get
  }

  override def getFile(path: CoNLLPath): Try[CoNLLFile] =
    Try(getFileUnsafe(path))

  def ptbToCoNLLPath(ptbPath: PTBPath): Try[CoNLLPath] = {
    val wsjPathRegex = """(.*)\.MRG""".r
    val wsjPathRegex(suffix) = ptbPath.suffix
    val conllPath = CoNLLPath(s"nw/wsj/${suffix.toLowerCase}.v4_gold_conll")
    getFile(conllPath).map(_ => conllPath)
  }

  def ptbToCoNLLSentencePath(ptbPath: PTBSentencePath): Try[CoNLLSentencePath] =
    ptbToCoNLLPath(ptbPath.filePath).map(CoNLLSentencePath(_, ptbPath.sentenceNum))
}
