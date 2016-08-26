package mts.util

import mts.core._
import mts.tasks.Config

import java.nio.file.{Paths, Path, Files}
import scala.util.Try

import upickle.default.Writer
import upickle.default.Reader
import upickle.default.write
import upickle.default.read

/**
  * Utility object for saving and loading files.
  * NOTE: the way we use FileManager, not all of the methods you see called on it will be here!
  * Instead, each package (conll, experiments, etc) may add extension methods to FileManager
  * via an implicit value class, to interface with files in ways appropriate to that package.
  * You can see these extension methods in each package's `package.scala`.
  * This way file managing functionality is logically separated, but narrowly namespaced.
  * I'm not totally confident that this is the best way of doing things...
  * but I kind of like it better than having a monolithic thing with a bunch of random functionality. -J
  */
object FileManager {
  private[this] val mTurkAnnotationPath = Paths.get("annotations").resolve(Config.label)
  private[this] val hitFilename = Paths.get("hit.txt")
  private[this] val rejectionDirectory = Paths.get("rejections")

  val resourcePath = Paths.get("resources")

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

  def saveHIT[Prompt : Writer](hit: HIT[Prompt]): Try[Unit] = Try {
    val directory = getHITPath(hit.hitType, hit.hitId)
    val savePath = directory.resolve(hitFilename)
    val serializedHIT = write(hit)
    Files.write(savePath, serializedHIT.getBytes());
  }

  def loadHIT[Prompt : Reader](path: Path): Try[HIT[Prompt]] = Try {
    import scala.collection.JavaConverters._
    val fileStream = Files.lines(path.resolve(hitFilename))
    val fileStr = fileStream.iterator.asScala.mkString("\n")
    fileStream.close()
    read[HIT[Prompt]](fileStr)
  }

  def getHIT[Prompt : Reader](hitType: String, hitId: String): Try[HIT[Prompt]] =
    loadHIT(getHITPath(hitType, hitId))

  def saveAssignment[Response : Writer](assignment: Assignment[Response]): Try[Unit] = Try {
    val directory = getHITPath(assignment.hitType, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    val serializedAssignment = write(assignment)
    Files.write(savePath, serializedAssignment.getBytes());
  }

  def saveRejectedAssignment[Response : Writer](assignment: Assignment[Response]): Try[Unit] = Try {
    val directory = getRejectionPath(assignment.hitType, assignment.hitId)
    val savePath = directory.resolve(s"${assignment.assignmentId}.txt")
    val serializedAssignment = write(assignment)
    Files.write(savePath, serializedAssignment.getBytes());
  }

  def loadAssignment[Response : Reader](path: Path): Try[Assignment[Response]] = Try {
    import scala.collection.JavaConverters._
    val fileStream = Files.lines(path)
    val fileStr = fileStream.iterator.asScala.mkString("\n")
    fileStream.close()
    read[Assignment[Response]](fileStr)
  }

  def loadAllData[Prompt: Reader, Response : Reader](
    hitType: String
  ): List[(HIT[Prompt], List[Assignment[Response]])] = {
    import scala.collection.JavaConverters._
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
    import scala.collection.JavaConverters._
    val hitPath = getHITPath(hitType, hitId)
    val assignments = for {
      file <- new java.io.File(hitPath.toString).listFiles
      if !file.isDirectory // exclude rejection directory
      if !file.getPath.toString.endsWith(hitFilename.toString)
      assignment <- loadAssignment[Response](Paths.get(file.getPath)).toOptionPrinting.toList
    } yield assignment
    assignments.toList
  }

  // XXX below this are older fields/methods used only for experiments A and B

  private[this] val questionFilePath = Paths.get("questions")

  private[this] def getQuestionPath(hitType: String, questionId: String) = {
    val hitTypePath = getHITTypePath(hitType)
    val questionPath = hitTypePath.resolve(questionId)
    if(!Files.exists(questionPath)) {
      Files.createDirectory(questionPath);
    }
    questionPath
  }

  private[this] def getQuestionStorePath(hitType: String) = {
    getHITTypePath(hitType).resolve(questionFilePath)
  }

  def saveAnnotation(annotation: Annotation): Try[Unit] = Try {
    val directory = getQuestionPath(annotation.hitType, annotation.question.hashCode.toString)
    val savePath = directory.resolve(s"${annotation.hitId}-${annotation.assignmentId}.txt")
    val serializedAnnotation = write(annotation)
    Files.write(savePath, serializedAnnotation.getBytes());
  }

  def loadAnnotation(path: Path): Try[Annotation] = Try {
    import scala.collection.JavaConverters._
    val fileStream = Files.lines(path)
    val fileStr = fileStream.iterator.asScala.mkString("\n")
    fileStream.close()
    read[Annotation](fileStr)
  }

  def loadAnnotationsForHITType(hitType: String): List[Annotation] = {
    val hitTypePath = getHITTypePath(hitType)
    val annotations = for {
      subfolder <- new java.io.File(hitTypePath.toString).listFiles
      if subfolder.isDirectory // exclude the "questions" file if necessary
      annotationFile <- subfolder.listFiles
      annotation <- loadAnnotation(Paths.get(annotationFile.getPath)).toOptionPrinting
    } yield annotation
    annotations.toList
  }

  def saveQuestionStore(hitType: String, qStore: Map[String, Question]) = {
    val qStorePath = getQuestionStorePath(hitType)
    val serializedQStore = write(qStore)
    Files.write(qStorePath, serializedQStore.getBytes())
  }

  def loadQuestionStore(hitType: String): Option[Map[String, Question]] = {
    val qStorePath = getQuestionStorePath(hitType)
    if(Files.exists(qStorePath)) {
      import scala.collection.JavaConverters._
      val fileStream = Files.lines(qStorePath)
      val fileStr = fileStream.iterator.asScala.mkString("\n")
      fileStream.close()
      Some(read[Map[String, Question]](fileStr))
    } else {
      None
    }
  }
}
