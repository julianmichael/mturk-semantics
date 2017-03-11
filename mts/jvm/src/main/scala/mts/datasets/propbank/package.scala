package mts.datasets
package propbank

import mts.util.FileManager

import java.nio.file.Paths

import scala.util.Try

trait PackagePlatformExtensions {

  def ptbToPropBankPath(ptbPath: ptb.PTBPath): Option[PropBankPath] = {
    val wsjPathRegex = """(.*)\.MRG""".r
    val wsjPathRegex(suffix) = ptbPath.suffix
    val pbPath = PropBankPath(s"ontonotes/nw/wsj/${suffix.toLowerCase}")
    FileManager.getPropBankFile(pbPath).toOption.map(_ => pbPath)
  }
  def ptbToPropBankSentencePath(ptbPath: ptb.PTBSentencePath): Option[PropBankSentencePath] =
    ptbToPropBankPath(ptbPath.filePath).map(
      PropBankSentencePath(_, ptbPath.sentenceNum)
    )

  import scala.language.implicitConversions
  implicit def fileManagerToPropBank(fm: FileManager.type): PropBankFileManager.type = PropBankFileManager

  object PropBankFileManager {
    private[this] val propBankAnnotationPath = Paths.get("propbank-release-master/data")

    import com.softwaremill.macmemo.memoize
    import com.softwaremill.macmemo.MemoCacheBuilder
    implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

    // always gets the conll formatted one
    @memoize(maxSize = 200, expiresAfter = 1 hour)
    private[this] def getPropBankFileUnsafe(path: PropBankPath): PropBankFile = {
      val fullPath = propBankAnnotationPath.resolve(s"${path.get}.gold_conll")
      val fileResource = FileManager.loadResource(fullPath).map(Parsing.readFile(path, _))
      fileResource.tried.get
    }

    def getPropBankFile(path: PropBankPath): Try[PropBankFile] =
      Try(getPropBankFileUnsafe(path))

    def getPropBankSentence(path: PropBankSentencePath): Try[PropBankSentence] = for {
      file <- getPropBankFile(path.filePath)
    } yield file.sentences(path.sentenceNum)
  }
}
