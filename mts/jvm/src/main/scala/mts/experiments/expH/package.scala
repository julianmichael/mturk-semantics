package mts.experiments.expH

import akka.actor.ActorRef

import java.nio.file.Paths

import mts.util._
import mts.util.LowerCaseStrings._
import mts.core._
import mts.datasets.conll._
import mts.datasets.ptb._
import mts.language._

trait PackagePlatformExtensions {
  // def getSentenceById(id: SentenceId): Vector[String] = id match {
  //   case CoNLLSentenceId(path) => FileManager.getCoNLLSentence(path).get.words.map(_.token).toVector
  //     // TODO wiki case when implemented
  // }

  def getWordsInQuestion(sentence: PTBSentence, string: String)(implicit inflections: Inflections): Set[Int] = {
    val tokens = tokenize(string).filterNot(isReallyUninteresting)
    val moreTokens = tokens.map(t => TextRendering.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.words.filter(w => generalizedTokens.contains(w.token.lowerCase)).map(_.index).toSet
  }

  def splitNum(n: Int): List[Int] =
    if(n <= 0) Nil
    else if(n <= 3) List(n)
    else if(n == 5) List(2, 3)
    else if(n == 6) List(3, 3)
    else if(n == 9) List(3, 3, 3)
    else 4 :: splitNum(n - 4)

  def splitList(l: List[Int]) = splitNum(l.size)
    .foldLeft((l, List.empty[List[Int]])) {
    case ((remaining, groups), groupSize) =>
      (remaining.drop(groupSize), remaining.take(groupSize) :: groups)
  }._2

  def pathSplits(path: PTBSentencePath) = {
    val tokens = getPTBTokens(path)
    splitList(tokens.indices.filter(i => !isReallyUninteresting(tokens(i))).toList)
  }


  def getPTBTokens(path: PTBSentencePath): Vector[String] = {
    val sentence = FileManager.getPTBSentence(path).get
    getPTBSentenceTokens(sentence)
  }

  case class ValidationResult(
    prompt: GenerationPrompt,
    sourceHITId: String,
    sourceAssignmentId: String,
    numValid: Int)

  case object SaveData

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
