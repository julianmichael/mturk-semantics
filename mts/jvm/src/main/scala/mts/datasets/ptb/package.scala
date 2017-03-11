package mts.datasets.ptb

import mts.util._
import mts.datasets.conll._
import mts.language._

import scala.util.Try
import scala.language.implicitConversions

import java.nio.file.{Paths, Path, Files}

trait PackagePlatformExtensions {

  implicit def fileManagerToPTB(fm: FileManager.type) = PTBFileManager

  object PTBFileManager {
    private[this] val ptbAnnotationPath = Paths.get("ptb/COMBINED/WSJ")

    import com.softwaremill.macmemo.memoize
    import com.softwaremill.macmemo.MemoCacheBuilder
    implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

    @memoize(maxSize = 200, expiresAfter = 1 hour)
    private[this] def getPTBFileUnsafe(path: PTBPath): PTBFile = {
      val fullPath = ptbAnnotationPath.resolve(path.suffix)
      val fileResource = for {
        lines <- FileManager.loadResource(fullPath)
        file = Parsing.readFile(lines)
      } yield file
      fileResource.tried.get
    }

    def getPTBFile(path: PTBPath): Try[PTBFile] =
      Try(getPTBFileUnsafe(path))

    def getPTBSentence(path: PTBSentencePath): Try[PTBSentence] = for {
      file <- getPTBFile(path.filePath)
    } yield file.sentences(path.sentenceNum)

    def allPTBPaths = {
      val prefix = FileManager.getResourcePath.resolve(ptbAnnotationPath)
      for {
        sectionName <- new java.io.File(prefix.toString).listFiles.map(_.getName).iterator
        sectionFolder =  new java.io.File(prefix.resolve(sectionName).toString)
        if sectionFolder.isDirectory
        fileName <- sectionFolder.listFiles.map(_.getName).iterator
      } yield PTBPath(s"$sectionName/$fileName")
    }

    def allPTBSentencePaths = allPTBPaths.flatMap { ptbPath =>
      getPTBFile(ptbPath).get.sentences.indices.iterator.map(i => PTBSentencePath(ptbPath, i))
    }
  }

  import PTBFileManager._

  def qaSRLPTBSentenceTokens = FileManager.loadResource(Paths.get("qasrl_train_sents_c09.txt"))
    .map(_.toList).tried.get
    .map(_.split(" "))

  def findQASRLPTBPaths = {
    import scala.collection.mutable
    val paths = mutable.Set.empty[PTBSentencePath]
    val sentencesNoSpaces = mutable.Set.empty[String]
    qaSRLPTBSentenceTokens
      .map(_.map(TextRendering.normalizeToken).mkString("").replaceAll("\\s", ""))
      .foreach(s => sentencesNoSpaces += s)

    allPTBSentencePaths.foreach { sPath =>
      val sentence = TextRendering.renderSentence(getPTBSentence(sPath).get).replaceAll("\\s", "")
      if(sentencesNoSpaces.contains(sentence)) {
        sentencesNoSpaces -= sentence
        paths += sPath
        println(sPath)
      }
    }

    (paths.toSet, sentencesNoSpaces.toSet)
  }
}
