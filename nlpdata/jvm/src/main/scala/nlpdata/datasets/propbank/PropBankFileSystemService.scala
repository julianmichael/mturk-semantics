package nlpdata.datasets
package propbank

import nlpdata.util._

import cats.Monad
import cats.implicits._

import scala.util.Try

import java.nio.file.{Paths, Path, Files}

class PropBankFileSystemService(location: Path) extends PropBankService[Try] {
  implicit override val monad: Monad[Try] = implicitly[Monad[Try]]

  private[this] val propBankAnnotationPath = location.resolve("data")

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder
  import scala.concurrent.duration._

  // always gets the conll formatted one
  @memoize(maxSize = 200, expiresAfter = 1.hour)
  private[this] def getFileUnsafe(path: PropBankPath): PropBankFile = {
    val fullPath = propBankAnnotationPath.resolve(s"${path.get}.gold_conll")
    val fileResource = loadFile(fullPath).map(Parsing.readFile(path, _))
    fileResource.tried.get
  }

  override def getFile(path: PropBankPath): Try[PropBankFile] =
    Try(getFileUnsafe(path))

  def ptbToPropBankPath(ptbPath: ptb.PTBPath): Try[PropBankPath] = {
    val wsjPathRegex = """(.*)\.MRG""".r
    val wsjPathRegex(suffix) = ptbPath.suffix
    val pbPath = PropBankPath(s"ontonotes/nw/wsj/${suffix.toLowerCase}")
    getFile(pbPath).map(_ => pbPath)
  }

  def ptbToPropBankSentencePath(ptbPath: ptb.PTBSentencePath): Try[PropBankSentencePath] =
    ptbToPropBankPath(ptbPath.filePath).map(
      PropBankSentencePath(_, ptbPath.sentenceNum)
    )
}
