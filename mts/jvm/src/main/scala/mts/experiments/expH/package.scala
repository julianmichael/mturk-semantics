package mts.experiments.expH

import java.nio.file.Paths

import mts.util._
import mts.conll._
import mts.ptb._
import mts.language._

trait PackagePlatformExtensions {
  def getSentenceById(id: SentenceId): Vector[String] = id match {
    case CoNLLSentenceId(path) => FileManager.getCoNLLSentence(path).get.words.map(_.token).toVector
      // TODO wiki case when implemented
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
