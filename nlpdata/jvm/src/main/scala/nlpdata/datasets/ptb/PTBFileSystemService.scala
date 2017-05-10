package nlpdata.datasets.ptb

import cats.Monad
import cats.implicits._

import nlpdata.util._

import scala.util.Try
import scala.language.implicitConversions

import java.nio.file.{Paths, Path, Files}

class PTBFileSystemService(location: Path) extends PTBService[Try] {
  implicit val monad: Monad[Try] = implicitly[Monad[Try]]
  private[this] val wsjAnnotationPath = location.resolve(Paths.get("COMBINED/WSJ"))

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  private[this] implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder
  import scala.concurrent.duration._
  import scala.language.postfixOps

  @memoize(maxSize = 200, expiresAfter = 1 hour)
  private[this] def getFileUnsafe(path: PTBPath): PTBFile = {
    val fullPath = wsjAnnotationPath.resolve(path.suffix)
    val fileResource = loadFile(fullPath).map(Parsing.readFile)
    fileResource.tried.get
  }

  override def getFile(path: PTBPath): Try[PTBFile] = Try(getFileUnsafe(path))

  def allPTBPaths = Try {
    val pathsIter = for {
      sectionName <- new java.io.File(wsjAnnotationPath.toString).listFiles.map(_.getName).iterator
      sectionFolder = new java.io.File(wsjAnnotationPath.resolve(sectionName).toString)
      if sectionFolder.isDirectory
      fileName <- sectionFolder.listFiles.map(_.getName).iterator
    } yield PTBPath(s"$sectionName/$fileName")
    pathsIter.toList
  }
}
