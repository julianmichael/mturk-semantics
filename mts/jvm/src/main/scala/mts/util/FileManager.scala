package mts.util

import mts.core._
import mts.tasks.TaskConfig

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
  * Instead, each package (conll, experiments, etc) may add methods to FileManager
  * via an implicit conversions, to interface with files in ways appropriate to that package.
  * You can see these conversions in each package's `package.scala`.
  * This way file managing functionality is logically separated, but narrowly namespaced.
  * I'm not totally confident that this is the best way of doing things...
  * but I kind of like it better than having a monolithic thing with a bunch of random functionality. -J
  */
object FileManager {

  // == Basic auxiliary methods ==

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

  // NOTE: from here on we have stuff relevant only to HITs/Assignments.
  // Perhaps that doesn't belong in the util package?
  // TODO: decide whether to move this stuff somewhere else.

  // == Useful paths ==

  private[this] def mTurkAnnotationPath(implicit config: TaskConfig) = Paths.get("annotations").resolve(config.label)
  private[this] val hitFilename = Paths.get("hit.txt")
  private[this] val rejectionDirectory = Paths.get("rejections")
  private[this] val resourcePath = Paths.get("resources")

  // == Path accessors ==

  // Convenience methods to get file paths and create missing directories if necessary.

  private[this] def getHITTypePath(hitTypeId: String)(implicit config: TaskConfig) = {
    val hitTypePath = mTurkAnnotationPath.resolve(hitTypeId)
    if(!Files.exists(hitTypePath)) {
      Files.createDirectories(hitTypePath);
    }
    hitTypePath
  }

  private[this] def getHITPath(hitTypeId: String, hitId: String)(implicit config: TaskConfig) = {
    val hitTypePath = getHITTypePath(hitTypeId)
    val hitPath = hitTypePath.resolve(hitId)
    if(!Files.exists(hitPath)) {
      Files.createDirectories(hitPath);
    }
    hitPath
  }

  private[this] def getRejectionPath(hitTypeId: String, hitId: String)(implicit config: TaskConfig) = {
    val hitPath = getHITPath(hitTypeId, hitId)
    val rejectionPath = hitPath.resolve(rejectionDirectory)
    if(!Files.exists(rejectionPath)) {
      Files.createDirectories(rejectionPath);
    }
    rejectionPath
  }

  // == Saving / loading specific kinds of files ==

  /** Loads an iterator over the lines of the desired resource file, wrapped in an ARM container.
    *
    * @param the path of the file within the resources directory
    * @return the lines in the file, wrapped in ManagedResource for monadic use
    */
  def loadResource(path: Path): ManagedResource[Iterator[String]] =
    loadFile(resourcePath.resolve(path))

  /** Loads a JavaScript file compiled from the Scala.js portion of this project.
    *
    * @param the name of the JavaScript file
    * @return the lines in the file
    */
  def loadJavaScript(name: String): ManagedResource[Iterator[String]] = {
    val resource = getClass.getResourceAsStream(s"/$name")
    managed(scala.io.Source.fromInputStream(resource)).map(_.getLines)
  }

  /** Saves a HIT to disk.
    *
    * Idempotent, since the path at which it's saved is determined by the HIT itself.
    * If there is an error, returns it wrapped in a Try.
    *
    * @param hit the HIT to save
    * @return Success(()) if success, Failure(error) with the error if there is one
    */
  def saveHIT[Prompt : Writer](hit: HIT[Prompt])(implicit config: TaskConfig): Try[Unit] = {
    val savePath = getHITPath(hit.hitTypeId, hit.hitId).resolve(hitFilename)
    saveFile(savePath, write(hit))
  }

  // imports / implicits for easy memoization (below)
  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  private[this] implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

  /** Loads a HIT from a file, caching it if successful and otherwise throwing the exception.
    *
    * Here we don't wrap the result in a Try because we only want to cache successful loads.
    * This acts as an auxiliary method for the public-facing method getHIT.
    *
    * @param path the path to the HIT save file
    * @return the HIT loaded from the file
    */
  @memoize(maxSize = 500, expiresAfter = 12 hours)
  private[this] def loadHITUnsafe[Prompt : Reader](path: Path): HIT[Prompt] =
    loadSerialized[HIT[Prompt]](path.resolve(hitFilename)).get

  /** Loads a HIT from disk, finding it from its HIT type and HIT ID.
    *
    * If there is a failure in finding the file or deserializing it, returns a Failure in a Try.
    * If successful, the returned HIT will have the same HIT Type ID and HIT ID as passed in.
    *
    * @param hitTypeId the HIT Type ID of the desired HIT
    * @return hitId the HIT ID of the desired HIT
    */
  def getHIT[Prompt : Reader](hitTypeId: String, hitId: String)(implicit config: TaskConfig): Try[HIT[Prompt]] =
    Try(loadHITUnsafe(getHITPath(hitTypeId, hitId)))

  /** Saves an approved assignment annotation to disk.
    *
    * Idempotent, as an assignment carries all of the information necessary to determine its path on disk.
    * Should only be used for approved assignments; rejected assignments are placed differently.
    *
    * @param assignment the assignment to save
    * @return Success if saved successfully, otherwise Failure with the error
    */
  def saveApprovedAssignment[Response : Writer](assignment: Assignment[Response])(implicit config: TaskConfig): Try[Unit] = Try {
    val directory = getHITPath(assignment.hitTypeId, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    saveFile(savePath, write(assignment))
  }

  /** Saves a rejected assignment annotation to disk.
    *
    * Idempotent, as an assignment carries all of the information necessary to determine its path on disk.
    * Requires a upickle serializer for Response.
    * Should only be used for rejected assignments; approved assignments are placed differently.
    *
    * @param assignment the rejected assignment to save
    * @return Success if saved successfully, otherwise Failure with the error
    */
  def saveRejectedAssignment[Response : Writer](assignment: Assignment[Response])(implicit config: TaskConfig): Try[Unit] = Try {
    val directory = getRejectionPath(assignment.hitTypeId, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    saveFile(savePath, write(assignment))
  }

  /** Loads all saved HITs and Assignments from disk for a HIT type.
    *
    * Captures any exceptions, prints their info to console, and excludes the missed results (if any) from the loaded data.
    * Returns a list of (hit, assignments) pairs grouping each HIT with all of its assignments.
    *
    * @param hitTypeId the HIT Type ID to load all of the data for
    * @return all successfully loaded data for the HIT type
    */
  def loadAllData[Prompt: Reader, Response : Reader](
    hitTypeId: String
  )(implicit config: TaskConfig): List[(HIT[Prompt], List[Assignment[Response]])] = {
    val hitTypePath = getHITTypePath(hitTypeId)
    val allData = for {
      hitFolder <- new java.io.File(hitTypePath.toString).listFiles
      if hitFolder.isDirectory // exclude extraneous files if necessary --- shouldn't happen though
      hit <- Try(loadHITUnsafe[Prompt](Paths.get(hitFolder.getPath))).toOptionPrinting.toList
      assignments = loadAssignmentsForHIT[Response](hit.hitTypeId, hit.hitId)
    } yield (hit, assignments)
    allData.toList
  }

  // updated version of the above
  def loadAllHITInfo[Prompt: Reader, Response : Reader](
    hitTypeId: String
  )(implicit config: TaskConfig): List[HITInfo[Prompt, Response]] = {
    val hitTypePath = getHITTypePath(hitTypeId)
    val allData = for {
      hitFolder <- new java.io.File(hitTypePath.toString).listFiles
      if hitFolder.isDirectory // exclude extraneous files if necessary --- shouldn't happen though
      hit <- Try(loadHITUnsafe[Prompt](Paths.get(hitFolder.getPath))).toOptionPrinting.toList
      assignments = loadAssignmentsForHIT[Response](hit.hitTypeId, hit.hitId)
    } yield HITInfo(hit, assignments)
    allData.toList
  }

  /** Loads all approved assignments for the given HIT info.
    *
    * Captures any exceptions, prints them to the console, and excludes the missed results (if any) from the loaded data.
    *
    * @param hitTypeId the HIT Type ID of the HIT for which we want assignments
    * @param hitId the HIT ID of the HIT
    * @return all approved assignments for the specified HIT
    */
  def loadAssignmentsForHIT[Response : Reader](hitTypeId: String, hitId: String)(implicit config: TaskConfig): List[Assignment[Response]] = {
    val hitPath = getHITPath(hitTypeId, hitId)
    val assignments = for {
      file <- new java.io.File(hitPath.toString).listFiles
      if !file.isDirectory // exclude rejection directory
      if !file.getPath.toString.endsWith(hitFilename.toString)
      assignment <- loadSerialized[Assignment[Response]](Paths.get(file.getPath)).toOptionPrinting.toList
    } yield assignment
    assignments.toList
  }
}
