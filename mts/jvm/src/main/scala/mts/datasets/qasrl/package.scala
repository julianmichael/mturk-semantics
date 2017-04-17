package mts.datasets.qasrl

import mts.util._
import mts.datasets.ptb._
import mts.datasets.propbank.{PredicateArgumentStructure, Predicate, ArgumentSpan}
import mts.language._

import scala.util.Try
import scala.language.implicitConversions

import java.nio.file.{Paths, Path, Files}
import upickle.default._

trait PackagePlatformExtensions {

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

    FileManager.allPTBSentencePaths.foreach { sPath =>
      val sentence = TextRendering.renderSentence(FileManager.getPTBSentence(sPath).get).replaceAll("\\s", "")
      if(sentencesNoSpaces.contains(sentence)) {
        sentencesNoSpaces -= sentence
        paths += sPath
        println(sPath)
      }
    }

    (paths.toSet, sentencesNoSpaces.toSet)
  }

  // for now, just stringify the questions... can make better later
  case class QASRLSentence(
    ptbPath: PTBSentencePath,
    tokensOriginal: List[String],
    predicateArgumentStructuresOriginal: List[PredicateArgumentStructure]
  ) {
    val wordsOriginal = tokensOriginal.zipWithIndex.map { case (token, index) =>
      Word(token = token, index = index, pos = "")
    }
    // XXX copied from propbank code; could factor into single place
    val (tokens, predicateArgumentStructures) = {
      val wordMapping = wordsOriginal.foldRight((List.empty[List[Word]], false)) {
        case (w, (ws, hasHyphen)) => ws match {
          case prevGroup :: rest if hasHyphen => ((w :: prevGroup) :: rest, false)
          case prevGroup :: rest if w.token == "-" => ((w :: prevGroup) :: rest, true)
          case prevGroups => (List(w) :: prevGroups, w.token == "-")
        }
      }._1.zipWithIndex.flatMap {
        case (group, newIndex) =>
          val newToken = group.map(_.token).mkString
          val newPos = group.last.pos
          val newWord = Word(token = newToken, pos = newPos, index = newIndex)
          group.map(_ -> newWord)
      }.toMap
      val newWords = wordsOriginal.map(wordMapping).toSet.toList.sortBy((w: Word) => w.index)
      val newPAS = predicateArgumentStructuresOriginal.map {
        case PredicateArgumentStructure(pred, args) =>
          PredicateArgumentStructure(
            pred.copy(head = wordMapping(pred.head)),
            args.map { case ArgumentSpan(label, span) =>
              ArgumentSpan(label, span.map(wordMapping).toSet.toList.sortBy((w: Word) => w.index))
            })
      }
      (newWords, newPAS)
    }
  }

  // case class QASRLLabel(
  //   wh: String,
  // )

  import scala.language.implicitConversions
  implicit def fileManagerToQASRL(fm: FileManager.type): QASRLFileManager.type = QASRLFileManager

  object QASRLFileManager {
    private[this] val qasrlAnnotationPath = Paths.get("qasrl/propbank-qasrl.txt")
    private[this] val qasrlPathsPath = Paths.get("qasrl/qasrl-paths.txt")

    lazy val allQASRLPaths = FileManager.loadResource(qasrlPathsPath)
      .map(lines => read[List[PTBSentencePath]](lines.next))
      .tried.recover { case _ =>
        FileManager.loadResource(qasrlAnnotationPath).map { lines =>
          val allPaths: List[PTBSentencePath] = lines.sliding(2)
            .filter(group => group(0).startsWith("PROPBANK"))
            .flatMap { group =>
            val tokens = group(1).split(" ").toVector
            val renderedSentence = TextRendering.renderSentence(tokens)
            val ptbPathOpt = FileManager.allPTBSentencePaths.find { path =>
              val otherSent = FileManager.getPTBSentence(path).get
              val otherSentRendered = TextRendering.renderSentence(otherSent)
              otherSentRendered == renderedSentence
            }
            if(ptbPathOpt.isEmpty) {
              println("Couldn't find match for sentence:\n" + renderedSentence)
            }
            ptbPathOpt
          }.toList
          FileManager.saveResource(qasrlPathsPath, write(allPaths))
          allPaths
        }.tried.get
    }.get

    // this is stupid and due to the lack of a proper builder for immutable.Map
    lazy val getQASRLUnsafe: collection.Map[PTBSentencePath, QASRLSentence] = {
      FileManager.loadResource(qasrlAnnotationPath).map { lines =>
        def unfoldFile(fileRemainder: List[String]): List[(PTBSentencePath, QASRLSentence)] = {
          if(fileRemainder.size <= 1) Nil else {
            val (entryLabel :: sentence :: entries, _ :: restOfFile) = fileRemainder.span(_.trim.nonEmpty)
            val tokens = sentence.split(" ").toList
            val renderedTokens = TextRendering.renderSentence(tokens)
            // println(renderedTokens)
            val ptbPathOpt = allQASRLPaths.find(path =>
              TextRendering.renderSentence(FileManager.getPTBSentence(path).get).toLowerCase == renderedTokens.toLowerCase
            )
            ptbPathOpt match {
              case None =>
                println("Could not align QA-SRL sentence:\n" + renderedTokens)
                unfoldFile(restOfFile)
              case Some(ptbPath) =>
                val paStructures = entries.unfoldList {
                  entry => entry match {
                    case Nil => None
                    case predLine :: questionsAndOtherPreds =>
                      val IntMatch(index) :: predToken :: IntMatch(numQs) :: Nil = predLine.split("\t").toList
                      val (questions, otherPreds) = questionsAndOtherPreds.splitAt(numQs)
                      val predicate = Predicate(
                        head = Word(index = index, token = tokens(index), pos = ""),
                        predicateLemma = predToken,
                        framesetId = "")
                      val arguments = questions.toList.flatMap { line =>
                        val (qCols, aCol) = line.split("\t").splitAt(8)
                        val question = TextRendering.renderSentence(qCols.toSeq.filterNot(_ == "_")).capitalize
                        val fullAnswerIndices = aCol.head.split("###").toList.map(_.trim).map { aTokenString =>
                          val indexInSentence = sentence.toLowerCase.indexOf(aTokenString.toLowerCase)
                          if(indexInSentence < 0) {
                            println("Answer not found in sentence. Answer:\n" + aTokenString)
                            println("Sentence:\n" + sentence)
                            Set.empty[Int]
                          } else {
                            val answerFirstIndex = sentence
                              .substring(0, indexInSentence)
                              .filter(_ == ' ').size
                            val answerLastIndex = answerFirstIndex + aTokenString.filter(_ == ' ').size
                            (answerFirstIndex to answerLastIndex).toSet
                          }
                        }.reduce(_ union _)
                        if(fullAnswerIndices.isEmpty) {
                          println("No answer indices for predicate " + predicate.head.token + " -- " + question + " -- (" + aCol.mkString(" # ") + ")")
                          println("Sentence:\n" + renderedTokens)
                          None
                        } else {
                          val answerWords = fullAnswerIndices
                            .map(i => Word(index = i, token = tokens(i), pos = ""))
                            .toList.sortBy(_.index)
                          Some(ArgumentSpan(label = question, words = answerWords))
                        }
                      }
                      if(arguments.isEmpty) {
                        println("No arguments for predicate: " + predicate.head.token)
                        println("Sentence:\n" + renderedTokens)
                      }
                      Some(((PredicateArgumentStructure(predicate, arguments), otherPreds)))
                  }
                }
                (ptbPath -> QASRLSentence(ptbPath, tokens, paStructures)) :: unfoldFile(restOfFile)
            }
          }
        }
        unfoldFile(lines.toList).toMap
      }.tried.get
    }

    def getQASRL: Try[collection.Map[PTBSentencePath, QASRLSentence]] =
      Try(getQASRLUnsafe)

    // def getNomBankPredArgStructures(path: PTBSentencePath): Try[List[PredicateArgumentStructure]] = for {
    //   sentence <- FileManager.getPTBSentence(path)
    //   nombank <- getNomBank
    // } yield nombank(path).map(getPredicateArgumentStructure(_, sentence.syntaxTree))

    // def getNomBankPredArgStructuresReindexed(path: PTBSentencePath): Try[List[PredicateArgumentStructure]] = for {
    //   sentence <- FileManager.getPTBSentence(path)
    //   nombank <- getNomBank
    // } yield nombank(path).map(getPredicateArgumentStructureReindexed(_, sentence.syntaxTree).get)
  }
}
