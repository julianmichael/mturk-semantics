package mts

import mts.core._
import mts.util.FileManager

import scala.util.Try

package object experiments {

  import java.nio.file.{Paths, Path, Files}
  private[this] val experimentRootPath = Paths.get("experiments")
  private[this] val dataPath = Paths.get("data")

  implicit class ExperimentFileManager(val fm: FileManager.type) extends AnyVal {
    def saveDataFile(
      experimentName: String,
      annotations: List[Annotation],
      extraStats: List[AnnotationStat]
    ) = Try {
      val tsvContents = Annotation.toTSV(annotations, extraStats)
      val directory = experimentRootPath.resolve(experimentName).resolve(dataPath)
      if(!Files.exists(directory)) {
        Files.createDirectories(directory)
      }
      val path = directory.resolve("annotations.tsv")
      Files.write(path, tsvContents.getBytes())
    }
  }
}
