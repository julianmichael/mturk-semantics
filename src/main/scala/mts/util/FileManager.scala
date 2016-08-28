package mts.util

import mts.core._
import mts.tasks.Config

import java.nio.file.{Paths, Path, Files}

import scala.util.Try
import scala.concurrent.duration._
import scala.language.postfixOps

import upickle.default.Writer
import upickle.default.Reader
import upickle.default.write
import upickle.default.read

import resource.managed
import resource.ManagedResource

/** Utility object for saving and loading files.
  *
  * NOTE: the way we use FileManager, not all of the methods you see called on it will be here!
  * Instead, each package (conll, experiments, etc) may add extension methods to FileManager
  * via an implicit conversions, to interface with files in ways appropriate to that package.
  * You can see these conversions in each package's `package.scala`.
  * This way file managing functionality is logically separated, but narrowly namespaced.
  * I'm not totally confident that this is the best way of doing things...
  * but I kind of like it better than having a monolithic thing with a bunch of random functionality. -J
  */
object FileManager {

  // == Fundamental methods ==

  private[this] def loadFile(path: Path): ManagedResource[Iterator[String]] = {
    import scala.collection.JavaConverters._
    managed(Files.lines(path)).map(_.iterator.asScala)
  }

  private[this] def loadSerialized[A : Reader](path: Path): Try[A] = {
    val res = for {
      lines <- loadFile(path)
    } yield Try(read[A](lines.mkString("\n")))
    res.tried.flatten
  }

  private[this] def saveFile(path: Path, contents: String): Try[Unit] =
    Try(Files.write(path, contents.getBytes))

  def loadResource(path: Path): ManagedResource[Iterator[String]] =
    loadFile(resourcePath.resolve(path))

  // == Useful paths ==

  private[this] val mTurkAnnotationPath = Paths.get("annotations").resolve(Config.label)
  private[this] val hitFilename = Paths.get("hit.txt")
  private[this] val rejectionDirectory = Paths.get("rejections")
  private[this] val resourcePath = Paths.get("resources")

  // == Path accessors ==

  private[this] def getHITTypePath(hitType: String) = {
    val hitTypePath = mTurkAnnotationPath.resolve(hitType)
    if(!Files.exists(hitTypePath)) {
      Files.createDirectories(hitTypePath);
    }
    hitTypePath
  }

  private[this] def getHITPath(hitType: String, hitId: String) = {
    val hitTypePath = getHITTypePath(hitType)
    val hitPath = hitTypePath.resolve(hitId)
    if(!Files.exists(hitPath)) {
      Files.createDirectories(hitPath);
    }
    hitPath
  }

  private[this] def getRejectionPath(hitType: String, hitId: String) = {
    val hitPath = getHITPath(hitType, hitId)
    val rejectionPath = hitPath.resolve(rejectionDirectory)
    if(!Files.exists(rejectionPath)) {
      Files.createDirectories(rejectionPath);
    }
    rejectionPath
  }

  // == Saving / Loading HITs and Assignments ==

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

  def saveHIT[Prompt : Writer](hit: HIT[Prompt]): Try[Unit] = {
    val savePath = getHITPath(hit.hitType, hit.hitId).resolve(hitFilename)
    saveFile(savePath, write(hit))
  }

  @memoize(maxSize = 500, expiresAfter = 12 hours)
  def loadHIT[Prompt : Reader](path: Path): Try[HIT[Prompt]] =
    loadSerialized[HIT[Prompt]](path.resolve(hitFilename))

  def getHIT[Prompt : Reader](hitType: String, hitId: String): Try[HIT[Prompt]] =
    loadHIT(getHITPath(hitType, hitId))

  def saveAssignment[Response : Writer](assignment: Assignment[Response]): Try[Unit] = Try {
    val directory = getHITPath(assignment.hitType, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    saveFile(savePath, write(assignment))
  }

  def saveRejectedAssignment[Response : Writer](assignment: Assignment[Response]): Try[Unit] = Try {
    val directory = getRejectionPath(assignment.hitType, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    saveFile(savePath, write(assignment))
  }

  def loadAssignment[Response : Reader](path: Path): Try[Assignment[Response]] =
    loadSerialized[Assignment[Response]](path)

  def loadAllData[Prompt: Reader, Response : Reader](
    hitType: String
  ): List[(HIT[Prompt], List[Assignment[Response]])] = {
    val hitTypePath = getHITTypePath(hitType)
    val allData = for {
      hitFolder <- new java.io.File(hitTypePath.toString).listFiles
      if hitFolder.isDirectory // exclude extraneous files if necessary --- shouldn't happen though
      hit <- loadHIT[Prompt](Paths.get(hitFolder.getPath)).toOptionPrinting.toList
      assignments = for {
        assignmentFile <- hitFolder.listFiles
        if !assignmentFile.isDirectory // exclude rejection directory
        if !assignmentFile.getPath.toString.endsWith(hitFilename.toString)
        assignment <- loadAssignment[Response](Paths.get(assignmentFile.getPath)).toOptionPrinting.toList
      } yield assignment
    } yield (hit, assignments.toList)
    allData.toList
  }

  def loadAssignmentsForHIT[Response : Reader](hitType: String, hitId: String): List[Assignment[Response]] = {
    val hitPath = getHITPath(hitType, hitId)
    val assignments = for {
      file <- new java.io.File(hitPath.toString).listFiles
      if !file.isDirectory // exclude rejection directory
      if !file.getPath.toString.endsWith(hitFilename.toString)
      assignment <- loadAssignment[Response](Paths.get(file.getPath)).toOptionPrinting.toList
    } yield assignment
    assignments.toList
  }
}
