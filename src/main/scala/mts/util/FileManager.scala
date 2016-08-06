package mts.util

import mts.core._

import java.nio.file.{Paths, Path, Files}
import scala.util.Try
import scala.collection.mutable

object FileManager {
  private[this] val mTurkAnnotationPath = Paths.get("annotations")
  private[this] val questionFilePath = Paths.get("questions")

  private[this] def getHITTypePath(hitType: String) = {
    val hitTypePath = mTurkAnnotationPath.resolve(hitType)
    if(!Files.exists(hitTypePath)) {
      Files.createDirectory(hitTypePath);
    }
    hitTypePath
  }

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
    val serializedAnnotation = upickle.write(annotation)
    Files.write(savePath, serializedAnnotation.getBytes());
  }

  def loadAnnotation(path: Path): Try[Annotation] = Try {
      import scala.collection.JavaConverters._
      val fileStr = Files.lines(path).iterator.asScala.mkString("\n")
      upickle.read[Annotation](fileStr)
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
    val serializedQStore = upickle.write(qStore)
    Files.write(qStorePath, serializedQStore.getBytes())
  }

  def loadQuestionStore(hitType: String): Option[Map[String, Question]] = {
    val qStorePath = getQuestionStorePath(hitType)
    if(Files.exists(qStorePath)) {
      import scala.collection.JavaConverters._
      val fileStr = Files.lines(qStorePath).iterator.asScala.mkString("\n")
      Some(upickle.read[Map[String, Question]](fileStr))
    } else {
      None
    }
  }
}
