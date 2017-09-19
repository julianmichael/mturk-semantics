package example.multitask

import cats.~>
import cats.Id
import cats.Order
import cats.implicits._

import java.nio.file.Paths
import java.nio.file.Files

import scala.util.Try

import turksem.qamr.GenerationPrompt
import turksem.qasrl._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.datasets.ptb3._
import nlpdata.datasets.qasrl
import nlpdata.structure.PredicateArgumentStructure
import nlpdata.structure.ArgumentSpan

import akka.pattern.ask
import scala.concurrent.duration._

class Exp2Data {

  val resourcePath = java.nio.file.Paths.get("resources")

  val PTB = {
    val getTry = new (Try ~> Id) {
      def apply[A](a: Try[A]): Id[A] = a.get
    }
    new InterpretedPTB3Service(
      getTry compose (new PTB3FileSystemInterpreter(resourcePath.resolve("ptb3")))
    )
  }

  implicit object PTB3SentencePathHasTokens extends HasTokens[PTB3SentencePath] {
    def getTokens(id: PTB3SentencePath): Vector[String] = PTB.getSentence(id).tokens
  }

  val hitDataService = new FileSystemHITDataService(Paths.get("annotations/multitask-exp2"))

  val genHITTypeId = "3F3JD5CN1MXB89I3404MMOQFUS2H0Q"
  val valHITTypeId = "3KBOIXB475RNG937KWXSCXE72N6Q5D"

  val staticDataPath = Paths.get("static-data/multitask/exp2")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("in")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def allGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[PTB3SentencePath], List[VerbQA]](genHITTypeId).get
  def allValInfos = hitDataService.getAllHITInfo[QASRLValidationPrompt[PTB3SentencePath], List[QASRLValidationAnswer]](valHITTypeId).get

  lazy val QASRL = new qasrl.QASRLFileSystemService(
    resourcePath.resolve("qasrl"),
    new nlpdata.datasets.ptb.PTBFileSystemService(resourcePath.resolve("ptb"))
  )

  lazy val ptb3QASRLPaths = QASRL.allQASRLPaths.flatMap(PTB3SentencePath.fromPTBSentencePath)

  lazy val allIds = ptb3QASRLPaths.drop(100).take(100).toVector

  def writeReadableTSV = {

    def writeId(id: PTB3SentencePath) = id.toString
    val tsv = DataIO.makeReadableQAPairTSV[PTB3SentencePath](
      ids = allIds.toList,
      writeId = writeId,
      anonymizeWorker = identity, // TODO
      genInfos = allGenInfos,
      valInfos = allValInfos)
    saveOutputFile("readable.tsv", tsv)
  }

  case class QAPair[A](question: String, answers: A)

  type Answers = List[Set[Int]]

  case class VerbComparison(
    verbIndex: Int,
    origQAs: List[QAPair[Answers]],
    newQAs: List[QAPair[List[Answers]]]) {

    val newToAlignedOrigIndex: Map[Int, Int] = newQAs.zipWithIndex.flatMap {
      case (QAPair(question, newAnswerResponses), index) =>
        origQAs.zipWithIndex.find(_._1.question == question).map(_._2).orElse {
          origQAs.zipWithIndex.map {
            case (QAPair(question, origAnswers), origIndex) =>
              origIndex -> newAnswerResponses.map(newAnswers =>
                SetMetric.intersectionOverUnion(newAnswers.foldK, origAnswers.foldK)
              ).meanOpt.getOrElse(0.0) }
            .maximumOption(Order.by[(Int, Double), Double](_._2))
            .filter(_._2 > 0.2) // filter threshold chosen by gut
            .map(_._1)
        }.map(index -> _)
    }.toMap

    val origToAlignedNewIndex: Map[Int, Int] = origQAs.zipWithIndex.flatMap {
      case (QAPair(question, origAnswers), index) =>
        newQAs.zipWithIndex.find(_._1.question == question).map(_._2).orElse {
          newQAs.zipWithIndex.map {
            case (QAPair(_, newAnswerResponses), newIndex) =>
              newIndex -> newAnswerResponses.map(newAnswers =>
                SetMetric.intersectionOverUnion(newAnswers.foldK, origAnswers.foldK)
              ).meanOpt.getOrElse(0.0) }
            .maximumOption(Order.by[(Int, Double), Double](_._2))
            .filter(_._2 > 0.2) // filter threshold chosen by gut
            .map(_._1)
        }.map(index -> _)
    }.toMap

    val numCoveredOrigQAs = newToAlignedOrigIndex.values.toSet.size
    val numCoveredNewQAs = newToAlignedOrigIndex.values.toSet.size
  }

  case class QASRLComparison(
    sentencePath: PTB3SentencePath,
    verbs: List[VerbComparison])

  def getSomewhatInvalidQuestions(vc: VerbComparison) = {
    vc.newQAs.filter(_.answers.size == 2)
  }

  def getVeryInvalidQuestions(vc: VerbComparison) = {
    vc.newQAs.filter(_.answers.size == 1)
  }

  def renderSomewhatInvalidQAs(comparison: QASRLComparison) = {
    val tokens = comparison.sentencePath.tokens
    val renderedBadQAs = comparison.verbs.flatMap(vc =>
      getSomewhatInvalidQuestions(vc).map { qa =>
        val answersString = qa.answers
          .map(a => a.map(Text.renderSpan(tokens, _)).mkString(" / "))
          .mkString(" ### ")
        s"${tokens(vc.verbIndex)} (${vc.verbIndex})\t${qa.question}\t$answersString"
      }
    )
    if(renderedBadQAs.nonEmpty) {
      Text.render(tokens) + "\n" + renderedBadQAs.mkString("\n") + "\n"
    } else ""
  }

  def renderVeryInvalidQAs(comparison: QASRLComparison) = {
    val tokens = comparison.sentencePath.tokens
    val renderedBadQAs = comparison.verbs.flatMap(vc =>
      getVeryInvalidQuestions(vc).map { qa =>
        val answersString = qa.answers
          .map(a => a.map(Text.renderSpan(tokens, _)).mkString(" / "))
          .mkString(" ### ")
        s"${tokens(vc.verbIndex)} (${vc.verbIndex})\t${qa.question}\t$answersString"
      }
    )
    if(renderedBadQAs.nonEmpty) {
      Text.render(tokens) + "\n" + renderedBadQAs.mkString("\n") + "\n"
    } else ""
  }

  lazy val qasrlComparisons = {
    val genInfos = allGenInfos
    val valInfos = allValInfos
    val qasrlMapping = QASRL.getQASRL.get
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId)
    val genInfosBySentenceId = allGenInfos.groupBy(_.hit.prompt.id)
    genInfosBySentenceId.toList.map { case (sid, genInfos) =>
      val origStructuresByVerbIndex = qasrlMapping(PTB3SentencePath.toPTBSentencePath(sid).get)
        .predicateArgumentStructures
        .groupBy(_.pred.head.index)
      val verbComparisons = genInfos
        .flatMap(_.assignments)
        .flatMap(assignment =>
        assignment.response.zip(
          valInfos
            .filter(_.hit.prompt.sourceAssignmentId == assignment.assignmentId)
            .flatMap(_.assignments)
            .map(_.response)
            .transpose))
        .groupBy(_._1.verbIndex).toList
        .map { case (verbIndex, qas) =>
          VerbComparison(
            verbIndex = verbIndex,
            origQAs = origStructuresByVerbIndex.get(verbIndex).getOrElse(Nil).flatMap {
              case PredicateArgumentStructure(_, args) => args.map {
                case ArgumentSpan(question, answerWords) =>
                  val spans: List[Set[Int]] = answerWords
                    .map(_.index)
                    .foldLeft(SetUnionFind.empty[Int]) { case (uf, index) =>
                      // I want to use the state monad but tbh this is probably easier to understand lol
                      var res = uf.add(index) // we have a new span
                      res = res.union(index, index - 1).getOrElse(res) // but maybe needs to merge behind
                      res = res.union(index, index + 1).getOrElse(res) // or ahead
                      res
                  }.sets.toList.sortBy(_.head)
                  QAPair(question, spans)
              }
            },
            newQAs = qas.map {
              case (VerbQA(_, question, genAnswers), valAnswers) =>
                QAPair(question, genAnswers :: valAnswers.flatMap(_.getAnswer).map(_.spans))
            }
          )
      }
      QASRLComparison(sid, verbComparisons)
    }
  }

  lazy val recallOfNewToOrig = {
    val (numCovered, numTotal) = qasrlComparisons
      .flatMap(_.verbs)
      .map(v => v.numCoveredOrigQAs -> v.origQAs.size)
      .foldLeft((0, 0)) { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2) }
    numCovered.toDouble / numTotal
  }

  lazy val recallOfOrigToNew = {
    val (numCovered, numTotal) = qasrlComparisons
      .flatMap(_.verbs)
      .map(v => v.numCoveredNewQAs -> v.newQAs.size)
      .foldLeft((0, 0)) { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2) }
    numCovered.toDouble / numTotal
  }

  lazy val proportionExtra = {
    val (numAligned, numTotal) = qasrlComparisons
      .flatMap(_.verbs)
      .map(v => v.newToAlignedOrigIndex.size -> v.newQAs.size)
      .foldLeft((0, 0)) { case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2) }
    numTotal.toDouble / numAligned
  }

  def writeComparisonTSV = {
    val sb = new StringBuilder
    for(QASRLComparison(sentencePath, verbComparisons) <- qasrlComparisons) {
      val sentenceTokens = sentencePath.tokens
      def renderAnswers(answers: List[Set[Int]]) =
        answers.map(Text.renderSpan(sentenceTokens, _)).mkString(" / ")
      sb.append(s"$sentencePath\t${nlpdata.util.Text.render(sentenceTokens)}\n")
      for(vc @ VerbComparison(verbIndex, origQAs, newQAs) <- verbComparisons.sortBy(_.verbIndex)) {
        // preimage of origQA under alignment
        val inverseQAAlignment: Map[Int, List[Int]] =
          vc.newToAlignedOrigIndex.toList
            .groupBy(_._2)
            .map { case (origIndex, pairs) => origIndex -> pairs.map(_._1).toList }
            .withDefaultValue(Nil)
        for((QAPair(origQuestion, origAnswers), origIndex) <- origQAs.zipWithIndex) {
          sb.append(s"\t${sentenceTokens(verbIndex)} ($verbIndex)\t$origQuestion\t")
          sb.append(renderAnswers(origAnswers))
          inverseQAAlignment(origIndex) match {
            case Nil =>
              sb.append("\n")
            case headNewAlignedIndex :: tailNewAlignedIndices =>
              sb.append("\t")
              val QAPair(headNewQuestion, headNewResponses) = newQAs(headNewAlignedIndex)
              val headResponseString = headNewResponses.map(renderAnswers).mkString("\t")
              sb.append(s"$headNewQuestion\t$headResponseString\n")
              for(newIndex <- tailNewAlignedIndices) {
                val QAPair(newQuestion, newResponses) = newQAs(newIndex)
                val responseString = newResponses.map(renderAnswers).mkString("\t")
                sb.append(s"\t${sentenceTokens(verbIndex)} ($verbIndex)\t\t\t$newQuestion\t$responseString\n")
              }
          }
        }
        for((QAPair(newQuestion, newResponses), newIndex) <- newQAs.zipWithIndex
            .filterNot(p => vc.newToAlignedOrigIndex.contains(p._2))) {
          val QAPair(newQuestion, newResponses) = newQAs(newIndex)
          val responseString = newResponses.map(renderAnswers).mkString("\t")
          sb.append(s"\t${sentenceTokens(verbIndex)} ($verbIndex)\t--\t--\t$newQuestion\t$responseString\n")
        }
      }
    }
    saveOutputFile("comparison.tsv", sb.toString)
  }
}

object Exp2Data {
  def init = new Exp2Data
}
