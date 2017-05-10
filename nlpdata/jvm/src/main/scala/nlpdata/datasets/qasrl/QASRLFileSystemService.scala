package nlpdata.datasets.qasrl

import nlpdata.util._
import nlpdata.datasets.ptb._
import nlpdata.structure._

import cats.Monad

import scala.util.{Try, Success, Failure}

import java.nio.file.{Paths, Path, Files}

import upickle.default._

class QASRLFileSystemService(
  location: Path,
  ptbService: PTBService[Try]
) extends QASRLService[Try] {

  implicit override val monad: Monad[Try] = implicitly[Monad[Try]]

  private[this] val qasrlAnnotationPath = location.resolve("propbank-qasrl.txt")
  private[this] val qasrlPathsPath = location.resolve("qasrl-paths.txt")

  lazy val allQASRLPaths = loadFile(qasrlPathsPath)
    .map(lines => read[List[PTBSentencePath]](lines.next))
    .tried.recover { case _ =>
      loadFile(qasrlAnnotationPath).map { lines =>
        val allPaths: List[PTBSentencePath] = lines.sliding(2)
          .filter(group => group(0).startsWith("PROPBANK"))
          .flatMap { group =>
          val tokens = group(1).split(" ").toVector
          val renderedSentence = Text.render(tokens)
          val ptbPathOpt = ptbService.allPTBSentencePaths.get.find { path =>
            ptbService.getSentence(path) match {
              case Success(otherSent) =>
                val otherSentRendered = Text.render(otherSent)
                otherSentRendered == renderedSentence
              case _ => false
            }
          }

          if(ptbPathOpt.isEmpty) {
            println("Couldn't find match for sentence:\n" + renderedSentence)
          }
          ptbPathOpt
        }.toList
        saveFile(qasrlPathsPath, write(allPaths))
        allPaths
      }.tried.get
  }.get

  // this is stupid and due to the lack of a proper builder for immutable.Map
  lazy val getQASRLUnsafe: collection.Map[PTBSentencePath, QASRLSentence] = {
    loadFile(qasrlAnnotationPath).map { lines =>
      def unfoldFile(fileRemainder: List[String]): List[(PTBSentencePath, QASRLSentence)] = {
        if(fileRemainder.size <= 1) Nil else {
          val (entryLabel :: sentence :: entries, _ :: restOfFile) = fileRemainder.span(_.trim.nonEmpty)
          val tokens = sentence.split(" ").toList
          val renderedTokens = Text.render(tokens)
          // println(renderedTokens)
          val ptbPathOpt = allQASRLPaths.find(path =>
            Text.render(
              ptbService.getSentence(path).get
            ).toLowerCase == renderedTokens.toLowerCase
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
                      val question = Text.render(qCols.toSeq.filterNot(_ == "_")).capitalize
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

  def qaSRLPTBSentenceTokens = loadFile(location.resolve("qasrl_train_sents_c09.txt"))
    .map(_.toList).tried.get
    .map(_.split(" "))

  def findQASRLPTBPaths = {
    import scala.collection.mutable
    val paths = mutable.Set.empty[PTBSentencePath]
    val sentencesNoSpaces = mutable.Set.empty[String]
    qaSRLPTBSentenceTokens
      .map(_.map(Text.normalizeToken).mkString("").replaceAll("\\s", ""))
      .foreach(s => sentencesNoSpaces += s)

    ptbService.allPTBSentencePaths.get.foreach { sPath =>
      val sentence = Text.render(ptbService.getSentence(sPath).get).replaceAll("\\s", "")
      if(sentencesNoSpaces.contains(sentence)) {
        sentencesNoSpaces -= sentence
        paths += sPath
        println(sPath)
      }
    }

    (paths.toSet, sentencesNoSpaces.toSet)
  }
}

