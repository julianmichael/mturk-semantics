package mts

import mts.core._
import mts.conll._
import mts.util._
import mts.language._

import scala.util.Try

/**
  * Package `experiments`
  * This is to hold subpackages for each of the individual turk experiments we're running,
  * using the rest of the code in `mts` as a framework.
  *
  * If you're wondering where expA and expB are, go back in time until they appear.
  * Since those experiments the framework has been considerably refactored,
  * so they needed to be removed to get rid of the cruft.
  * Translating them into the new framework would have been a huge pain,
  * because the refactor changed the way that annotation data is stored.
  *
  * In the package object here is just some common data and common methods that are useful for any experiment.
  */
package object experiments {

  val annotationFilepaths = List(
    "bn/abc/00/abc_0010.v4_gold_conll",
    "mz/sinorama/10/ectb_1010.v4_gold_conll",
    "bc/msnbc/00/msnbc_0000.v4_gold_conll",
    "nw/wsj/24/wsj_2400.v4_gold_conll",
    "nw/xinhua/00/chtb_0010.v4_gold_conll",
    "pt/nt/40/nt_4010.v4_gold_conll",
    "wb/eng/00/eng_0000.v4_gold_conll"
  ).map(CoNLLPath.apply)

  lazy val sentences: List[(CoNLLSentencePath, CoNLLSentence)] = {
    val allSentences = for {
      path <- annotationFilepaths.iterator
      file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
      sentence <- file.sentences
      if sentence.sentenceNum % 2 == 0 || sentence.sentenceNum % 5 == 0 // skip some of the sentences
      if sentence.words.size > 6 // don't do the super short sentences
    } yield (CoNLLSentencePath(path, sentence.sentenceNum), sentence)
    allSentences.take(100).toList
  }

  lazy val inflections = {
    val tokens = for {
      path <- annotationFilepaths.iterator
      file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
      sentence <- file.sentences
      word <- sentence.words
    } yield word.token
    getInflectionsForTokens(tokens)
  }

  import java.nio.file.{Paths, Path, Files}
  private[this] val experimentRootPath = Paths.get("experiments")
  private[this] val dataPath = Paths.get("data")

  implicit class ExperimentFileManager(val fm: FileManager.type) extends AnyVal {
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
  }
}
