package nlpdata.util

import resource.managed
import resource.ManagedResource

import scala.util.Try

import java.nio.file.{Paths, Path, Files}

trait PackagePlatformExtensions {
  def loadFile(path: Path): ManagedResource[Iterator[String]] = {
    import scala.collection.JavaConverters._
    managed(Files.lines(path)).map(_.iterator.asScala)
  }

  def saveFile(path: Path, contents: String): Try[Unit] =
    Try(Files.write(path, contents.getBytes))
}
