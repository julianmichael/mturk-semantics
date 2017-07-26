package turksem

import scala.util.Try

trait AnnotationDataService {

  def saveLiveData(name: String, contents: String): Try[Unit]

  def loadLiveData(name: String): Try[List[String]]
}

import java.nio.file.Path
import java.nio.file.Files

class FileSystemAnnotationDataService(dataPath: Path) extends AnnotationDataService {
  override def saveLiveData(name: String, contents: String): Try[Unit] = Try {
    val directory = dataPath
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  override def loadLiveData(name: String): Try[List[String]] = Try {
    val directory = dataPath
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }
}
