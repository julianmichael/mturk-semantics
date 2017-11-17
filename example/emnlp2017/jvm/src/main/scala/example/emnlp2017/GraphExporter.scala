package example.emnlp2017

import cats.data.State
import cats.data.StateT
import cats.data.NonEmptyList
import cats.implicits._

import turksem.qamr._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.util.Text
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.HasTokens.ops._

class GraphExporter {

  def getQuestionLabelSimple(qs: Set[String]): String = {
    qs.toVector
      .map(_.takeWhile(_ != ' ').lowerCase)
      .filter(whWords.contains)
      .groupBy(identity)
      .toVector.sortBy(-_._2.size)
      .headOption.fold("unk".lowerCase)(_._1)
  }

  def useWholeQuestion(qs: Set[String]): String = qs.headOption.getOrElse("unk")

  // just ptb all right now...
  lazy val questionLabelMapper = new QuestionLabelMapper(Datasets.trainDevPTB)

  def useNewLabels(sid: SentenceId): (Set[String] => Option[String]) = {
    val qtasWithStuff = questionLabelMapper.getLabeledQAsForId(sid)
    // println(qtasWithStuff.size)
    val labelsByQString = qtasWithStuff.groupBy(_._1.question.toLowerCase).map {
      case (question, qtasAndStuff) =>
        // println(s"$question \t ${qtasAndStuff.map(t => t._1.template.show -> t._2._2)}")
        question -> qtasAndStuff.map(t => t._2._2 + (if(t._2._3) "-".lowerCase else "".lowerCase))
    }

    (qs: Set[String]) => {
      NonEmptyList.fromList(
        (qs.toList.flatMap(q => labelsByQString.get(q).getOrElse(Nil)): List[LowerCaseString])
          .groupBy(identity).toVector
          .sortBy(-_._2.size)
          .toList
      ).map(nel =>
        nel.toList.takeWhile(_._2 == nel.head._2)
          .find(_._1.contains("/".lowerCase))
          .getOrElse(nel.head)
          ._1
      )
    }
  }

  def useNewLabelsWithUnk(sid: SentenceId) = useNewLabels(sid) andThen ((_: Option[String]).getOrElse("UNK"))

  def getSingleNewQuestionLabel(sid: SentenceId) = {
    ((x: String) => Set(x)) andThen useNewLabels(sid)
  }

  // lazy val train = Datasets.train
  // lazy val dev = Datasets.dev

  // def printExampleGraph(data: QAData[SentenceId], n: Int, wholeQ: Boolean = true): Unit = {
  //   val (sid, sqas) = data.sentenceToQAs.iterator.take(n + 1).toList.last
  //   val getQLabel = if(wholeQ) useWholeQuestion(_) else useNewLabels(sid)
  //   println(Text.render(sid))
  //   val inducer = new StructureInduction.GraphInducer(sid.tokens, sqas)
  //   val graph = inducer.standardQAMRGraph
  //   println(graph.prettyString(sid.tokens, getQLabel))
  // }

  def printPTBDevGraph(doc: Int, sent: Int, wholeQ: Boolean = false): Unit = {
    import nlpdata.datasets.ptb._
    val sid = PTBSentenceId(PTBSentencePath(PTBPath(f"00/WSJ_00$doc%02d.MRG"), sent))
    val sqas = Datasets.ptbAll.sentenceToQAs(sid)
    println(Text.render(sid))
    val inducer = new StructureInduction.GraphInducer(sid.tokens, sqas)
    val graph = inducer.standardQAMRGraph
    if(wholeQ) {
      println(graph.mapLabels(useWholeQuestion).prettyString(sid.tokens))
    } else {
      println(graph.mapLabels(useNewLabelsWithUnk(sid)).stringForm(sid.tokens).mkString(" "))
    }
  }

  import cats.effect.IO
  import cats.effect.implicits._
  type LabelTrackingIO[A] = StateT[IO, Map[String, Int], A]

  // TODO use streams
  def writeSentenceAndGraphFiles(label: String, data: QAData[SentenceId]): LabelTrackingIO[Unit] = {
    for {
      sentSB <-  StateT.pure[IO, Map[String, Int], StringBuilder](new StringBuilder)
      graphSB <- StateT.pure[IO, Map[String, Int], StringBuilder](new StringBuilder)
      _ <- data.sentenceToQAs.iterator.map { case (sid, sqas) =>
        for {
          g <- StateT.lift[IO, Map[String, Int], QAMRGraph[String]](
            IO {
              System.out.print(".")
              val inducer = new StructureInduction.GraphInducer(sid.tokens, sqas)
              val graph = inducer.standardQAMRGraph.mapLabels(useNewLabelsWithUnk(sid))
              sentSB.append(sid.tokens.mkString(" ") + "\n")
              graphSB.append(graph.stringForm(sid.tokens).mkString(" ") + "\n")
              graph
            }
          )
          localCounts = counts(
            g.paStructures.values.iterator
              .flatMap(_.args)
              .map(_._2)
              .toList
          )
          _ <- StateT.modify[IO, Map[String, Int]](_ |+| localCounts)
        } yield ()
      }.toList.sequence[LabelTrackingIO, Unit].as(())
      _ <- StateT.lift[IO, Map[String, Int], Unit](
        IO {
          saveOutputFile(s"$label-sents.txt", sentSB.toString)
          saveOutputFile(s"$label-graphs.txt", graphSB.toString)
        }
      )
    } yield ()
  }

  def writeAllGraphFiles(prefix: String) = {
    val st = for {
      _ <- writeSentenceAndGraphFiles(s"$prefix/dev", Datasets.dev)
      _ <- writeSentenceAndGraphFiles(s"$prefix/test", Datasets.test)
      _ <- writeSentenceAndGraphFiles(s"$prefix/ptb", Datasets.ptbAll)
      _ <- writeSentenceAndGraphFiles(s"$prefix/train", Datasets.train)
      labelCounts <- StateT.get[IO, Map[String, Int]]
      _ <- StateT.lift[IO, Map[String, Int], Unit](
        IO {
          println(labelCounts.size)
          println(labelCounts.map(_._2).sum)
          val sortedLabels = labelCounts.iterator.toVector.sortBy(-_._2)
          sortedLabels.take(20).foreach(println)
          sortedLabels.takeRight(20).foreach(println)
        }
      )
    } yield ()
    st.runEmptyA.unsafeRunSync
  }

  // def useNewLabels(sid: SentenceId): (Set[String] => Option[String]) = {
  //   val qtasWithStuff = questionLabelMapper.getLabeledQAsForId(sid)
  //   // println(qtasWithStuff.size)
  //   val labelsByQString = qtasWithStuff.groupBy(_._1.question.toLowerCase).map {
  //     case (question, qtasAndStuff) =>
  //       // println(s"$question \t ${qtasAndStuff.map(t => t._1.template.show -> t._2._2)}")
  //       question -> qtasAndStuff.map(t => t._2._2 + (if(t._2._3) "-" else ""))
  //   }

  //   (qs: Set[String]) => {
  //     NonEmptyList.fromList(
  //       (qs.toList.flatMap(q => labelsByQString.get(q.toLowerCase).getOrElse(Nil)): List[String])
  //         .groupBy(identity).toVector
  //         .sortBy(-_._2.size)
  //         .toList
  //     ).map(nel =>
  //       nel.toList.takeWhile(_._2 == nel.head._2)
  //         .find(_._1.contains("/"))
  //         .getOrElse(nel.head)
  //         ._1
  //     )
  //   }
  // }

  // TODO use streams
  def readPrelimDependencyGraphs(inputFilepath: String): List[(SentenceId, QAMRDependencyGraph[PrelimLabel], Vector[LemmaPosWord])] = {
    val lines = loadInputFile(inputFilepath).get
    case class Chunking(chunks: List[NonEmptyList[String]], curChunk: List[String]) {
      def addLine(line: String) = if(line.isEmpty) finishChunk else Chunking(chunks, line :: curChunk)
      def finishChunk = NonEmptyList.fromList(curChunk).fold(this)(x => Chunking(x.reverse :: chunks, Nil))
      def finishChunks = finishChunk.chunks.reverse
    }
    object Chunking {
      def empty = Chunking(Nil, Nil)
    }
    val sentenceStringLists = lines.foldLeft(Chunking.empty)(_ addLine _).finishChunks
    val graphsWithInfo = sentenceStringLists.map { sentenceWordsNel =>
      val Array(filepath, sentenceIndexStr) = sentenceWordsNel.head.tail.split("\t")
      val sid = SentenceId.fromString(filepath.replace("/", ":").replaceAll(".txt", "") + ":" + sentenceIndexStr)
      val (graph, lemmaWords) = QAMRDependencyGraph.readFromSemEvalFormattedLines[PrelimLabel](
        sentenceWordsNel.tail,
        (argLabelString: String) => {
          val labelSet = argLabelString.split("\\|\\|\\|").toSet
          labelSet.headOption
            .filter(_.startsWith("dep"))
            .fold(
            QuestionsLabel(
              labelSet.map(q =>
                (if(q.endsWith("?")) q else q + "?").lowerCase
              )
            ): PrelimLabel)(headLabel =>
              // TODO include all of them? meh
              AutoInducedLabel(labelSet.head.split("\\|\\|").head.lowerCase)
            )
        }
      )
      (sid, graph, lemmaWords)
    }
    graphsWithInfo
  }

  def mapLabelsInGraphTriples(
    graphsWithIds: List[(SentenceId, QAMRDependencyGraph[PrelimLabel], Vector[LemmaPosWord])]
  ): List[(SentenceId, QAMRDependencyGraph[LowerCaseString], Vector[LemmaPosWord])] = graphsWithIds.map {
    case (sid, prelimGraph, words) =>
      val allLabels = prelimGraph.toList.distinct
      val relabeledGraph = prelimGraph.mapIndexedLabelsWithInversion(questionLabelMapper.mapLabels(sid))
        // .map(p => /*if(p._2) p._1.takeWhile(_ != '/').lowerCase + "-".lowerCase else */p._1.toString.takeWhile(_ != '/').lowerCase) // don't render inversions for now
        .filter(s => !s.startsWith("unk".lowerCase))
      (sid, relabeledGraph, words)
  }

  def readLabelFixedDependencyGraphs(inputFilepath: String) =
    mapLabelsInGraphTriples(readPrelimDependencyGraphs(inputFilepath))

  def writeDependencyGraphs(graphsWithIds: List[(SentenceId, QAMRDependencyGraph[LowerCaseString], Vector[LemmaPosWord])], outputFilepath: String) = {
    val outSB = new StringBuilder
    outSB.append("#QAMR Dependencies\n")
    graphsWithIds.foreach { case (sid, graph, lemmaWords) =>
      val idStr = "#" + SentenceId.toString(sid) + "\n"
      val tokenLinesStr = QAMRDependencyGraph.makeSemEvalFormattedString(lemmaWords, graph)
      outSB.append(idStr)
      outSB.append(tokenLinesStr)
      outSB.append("\n")
    }
    saveOutputFile(outputFilepath, outSB.toString)
  }

  def supportCounts = {
    readPrelimDependencyGraphs(s"graphs/word/train_psd.txt").iterator
      .map(_._2)
      .map(g => counts(g.toList.collect{ case QuestionsLabel(s) => s.size }))
      .toList
      .combineAll
  }

  def writeAllDependencyGraphFiles = {
    val trainGraphTriples = readLabelFixedDependencyGraphs(s"graphs/word/train_psd.txt")
    val devGraphTriples = readLabelFixedDependencyGraphs(s"graphs/word/dev_psd.txt")
    val testGraphTriples = readLabelFixedDependencyGraphs(s"graphs/word/test_psd.txt")

    val labelCounts = (
      trainGraphTriples.iterator /* ++ devGraphTriples.iterator ++ testGraphTriples.iterator*/
    ).map(x => counts(x._2.toList)).toList.combineAll
    val labelsByCountDecreasing = labelCounts.toVector.sortBy(-_._2)
    println(s"Number of labels: ${labelCounts.size}")
    println(s"Number of labels occurring once: ${labelCounts.filter(_._2 == 1).size}")
    println(s"Number of labels occurring more than 10 times: ${labelCounts.filter(_._2 > 10).size}")
    println(s"Most common labels: ")
    labelsByCountDecreasing.take(50).foreach { case(label, count) =>
      println(f"$label%-15s $count%5d")
    }
    println
    println(s"Least common labels: ")
    labelsByCountDecreasing.takeRight(50).foreach { case(label, count) =>
      println(f"$label%-15s $count%5d")
    }

    writeDependencyGraphs(trainGraphTriples, s"graphs/word-relabeled/train.txt")
    writeDependencyGraphs(trainGraphTriples.take(30), s"graphs/word-relabeled/train_small.txt")
    writeDependencyGraphs(devGraphTriples, s"graphs/word-relabeled/dev.txt")
    writeDependencyGraphs(testGraphTriples, s"graphs/word-relabeled/test.txt")
  }

  // def makeCompleteTSV(ids: List[SentenceId]): String =
  //   makeQAPairTSV(ids, SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos)

  // def makeReadableTSV(
  //   ids: List[SentenceId],
  //   includeQA: (SentenceId, WordedQAPair, List[ValidationAnswer]) => Boolean = isQAGood): String =
  //   makeReadableQAPairTSV(ids, SentenceId.toString, workerAnonymizationMap, allGenInfos, allValInfos, includeQA)

  // def writeTSVs = {
  //   saveOutputFile("sentences.tsv", makeSentenceIndex(allIds: List[SentenceId], SentenceId.toString))
  //   saveOutputFile("train.tsv", makeCompleteTSV(trainIds))
  //   saveOutputFile("dev.tsv", makeCompleteTSV(devIds))
  //   saveOutputFile("test.tsv", makeCompleteTSV(testIds))
  //   saveOutputFile("train-readable.tsv", makeReadableTSV(trainIds))
  //   saveOutputFile("dev-readable.tsv", makeReadableTSV(devIds))
  //   saveOutputFile("test-readable.tsv", makeReadableTSV(testIds))
  // }

  // def writePTBTSVs = {
  //   saveOutputFile("ptb-sentences.tsv", makeSentenceIndex(allPTBIds, SentenceId.toString))
  //   saveOutputFile("ptb-train.tsv", makeCompleteTSV(ptbTrainIds))
  //   saveOutputFile("ptb-dev.tsv", makeCompleteTSV(ptbDevIds))
  //   saveOutputFile("ptb-test.tsv", makeCompleteTSV(ptbTestIds))
  //   saveOutputFile("ptb-amr.tsv", makeCompleteTSV(ptbAMRIds))
  //   saveOutputFile("ptb-readable.tsv", makeReadableTSV(allPTBIds))
  // }

  // def goodQAContainsOneSentenceWord(sid: SentenceId, wqa: WordedQAPair, valAnswers: List[ValidationAnswer]) = {
  //   val questionTokens = Tokenizer.tokenize(wqa.question)
  //   isQAGood(sid, wqa, valAnswers) && getWordsInQuestion(sid.tokens, questionTokens).size == 1
  // }
}
