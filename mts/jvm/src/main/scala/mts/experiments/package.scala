package mts.experiments

import mts.core._
import mts.util._

import nlpdata.datasets._
import nlpdata.datasets.wiktionary.Inflections

import scala.util.Try

import resource.managed
import resource.ManagedResource

trait PackagePlatformExtensions {

  val CoNLL = new conll.CoNLLFileSystemService(
    FileManager.getResourcePath.resolve("conll-2012")
  )

  val PTB = new ptb.PTBFileSystemService(
    FileManager.getResourcePath.resolve("ptb")
  )

  val PropBank = new propbank.PropBankFileSystemService(
    FileManager.getResourcePath.resolve("propbank-release-master")
  )

  val NomBank = new nombank.NomBankFileSystemService(
    FileManager.getResourcePath.resolve("nombank.1.0"), PTB
  )

  val QASRL = new qasrl.QASRLFileSystemService(
    FileManager.getResourcePath.resolve("qasrl"), PTB
  )

  val Wiki1k = new wiki1k.Wiki1kFileSystemService(
    FileManager.getResourcePath.resolve("wiki1k")
  )

  val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    FileManager.getResourcePath.resolve("wiktionary")
  )

  import java.nio.file.{Paths, Path, Files}
  private[this] val experimentRootPath = Paths.get("experiments")
  private[this] val dataPath = Paths.get("data")

  implicit class ExperimentFileManager(val fm: FileManager.type) {
    def saveDataFile(
      experimentName: String,
      fileName: String,
      contents: String
    ) = Try {
      val directory = experimentRootPath.resolve(experimentName).resolve(dataPath)
      if(!Files.exists(directory)) {
        Files.createDirectories(directory)
      }
      val path = directory.resolve(fileName)
      Files.write(path, contents.getBytes())
    }

    def loadDataFile(
      experimentName: String,
      fileName: String
    ) = {
      val directory = experimentRootPath.resolve(experimentName).resolve(dataPath)
      if(!Files.exists(directory)) {
        Files.createDirectories(directory)
      }
      val path = directory.resolve(fileName)
      import scala.collection.JavaConverters._
      managed(Files.lines(path)).map(_.iterator.asScala.toList).tried
    }
  }
}
