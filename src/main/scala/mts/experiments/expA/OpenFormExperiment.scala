package mts.experiments.expA

import mts.experiments._
import mts.analysis._
import mts.core._
import mts.util._
import mts.tasks._
import mts.conll._
import mts.language._
import mts.validation._
import akka.actor._
import mts.util.LowerCaseStrings._

import scala.util.Try

object OpenFormExperiment {
  // file name has A_ prepended so experiments have an obvious order
  val experimentName = "a_open-form"
  // get 100 sentences from conll data
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

  lazy val questionValidator = new SomeWordValidator(inflections)
  lazy val answerValidator = new AllWordsValidator(inflections)

  // bucket sentences and create a task for each bucket
  private[this] def makeTask(system: ActorSystem)(minTokens: Int, maxTokens: Int, numQAs: Int, reward: Double) = {
    val taskSpec = OpenFormTask(reward, numQAs, numAssignmentsPerHIT = 1)
    val filteredSentences = sentences.iterator
      .filter { case (_, sentence) => sentence.words.size >= minTokens && sentence.words.size <= maxTokens }
      .map { case (path, sentence) => (path, TextRendering.renderSentence(sentence)) }
    // val actor = taskSpec.createMonitor(system, filteredSentences, 100)
    val actor = system.actorOf(Props(TaskMonitor(taskSpec, filteredSentences, 100)))
    (taskSpec, actor)
  }

  lazy val system = ActorSystem("system")

  lazy val tasks = List[(OpenFormTask, ActorRef)](
    makeTask(system)(minTokens = 7, maxTokens = 18, numQAs = 4, reward = 0.20),
    makeTask(system)(minTokens = 19, maxTokens = 120, numQAs = 6, reward = 0.30)
  )

  val protoTaskSpec = OpenFormTask(0.0, 0, 0)

  def start() = tasks.foreach(p => p._2 ! p._1.Message.Start)
  def stop() = tasks.foreach(p => p._2 ! p._1.Message.Stop)
  def disable() = tasks.foreach(p => p._2 ! p._1.Message.Disable)
  def expire() = tasks.foreach(p => p._2 ! p._1.Message.Expire)
  def update() = tasks.foreach(p => p._2 ! p._1.Message.Update)

  // TODO save HIT types and access them here and such
  def getAllAnnotations(): List[Annotation] =
    // tasks.flatMap(p => FileManager.loadAnnotationsForHITType(p._1.hitType))
    List("3NWM3X0LA7LBIGM2QP1Y4BFHQN9XPC", "3JH21YRKZ6B7ZFDTS077Y16U7HJ0JL")
      .flatMap(FileManager.loadAnnotationsForHITType)

  def getAllQAPairs(): Iterable[(OpenFormPrompt, List[OpenFormResponse])] =
    tasks.flatMap(_._1.annotatedQAPairs).toMap.values

  def getAllFeedback(): Iterable[String] = getAllQAPairs().flatMap(_._2.map(_._2))
    .filterNot(_.isEmpty)

  // assume we have the question for everything ugh
  def annotationToInfo(anno: Annotation) = {
    import mts.language._
    val question = anno.question.get
    val ((path, _), (qaPairs, _)) = (protoTaskSpec.extractPrompt(question), protoTaskSpec.extractResponse(anno))
    val infos = for {
      sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
      (q, a) <- qaPairs
    } yield QAInfo(sentence, q, a, anno, 0)
    AggregatedQAInfo(List("assignmentId"), List(anno.assignmentId), infos.toList)
  }

  def readableQATSV(): String = {
    val annotations = getAllAnnotations().filter(!_.question.isEmpty)
    val annotationsToInfos = annotations.map(a => a -> annotationToInfo(a)).toMap
    val infoAggregatedByHIT = annotations.groupBy(_.hitId).map {
      case (hitId, annos) => hitId -> (annos, AggregatedQAInfo(List("hitId"), List(hitId), annos.flatMap(a => annotationToInfo(a).qas)))
    }

    val sb = new java.lang.StringBuilder()
    for {
      (annos, aggInfo) <- infoAggregatedByHIT.values
      _ = sb.append("\n")
      _ = sb.append(TextRendering.renderSentence(aggInfo.sentence) + "\n")
      _ = for {
        annotation <- annos
        worker = annotation.workerId
        (_, (qaPairs, feedback)) <- protoTaskSpec.getQAPair(annotation).toList
        _ = if(!feedback.isEmpty) sb.append(s"$worker\tFeedback: $feedback\n") else ()
        (q, a) <- qaPairs
        qValid = questionValidator.isValid(aggInfo.sentence.words.map(_.token.lowerCase), tokenize(q).map(_.lowerCase))
        qValidFlag = if(qValid) " " else "Q!"
        aValid = answerValidator.isValid(aggInfo.sentence.words.map(_.token.lowerCase), tokenize(a).map(_.lowerCase))
        aValidFlag = if(aValid) " " else "A!"
        _ = sb.append(s"$qValidFlag$aValidFlag\t$q\t$a\t$worker\n")
      } yield ()
      _ = for { // print dependencies with any formatting
        PredicateArgumentStructure(Predicate(head, _, _), args) <- aggInfo.sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        if !label.equals("V")
        covered = words.exists(spanWord => aggInfo.arcs.contains(head.index, spanWord.index) ||
                                 aggInfo.arcs.contains(spanWord.index, head.index))
        coveredFlag = if(covered) "V" else "X"
        spanPhrase = TextRendering.renderSentence(words.map(_.token))
        _ = sb.append(s"$coveredFlag\t${head.token}\t$label\t$spanPhrase\n")
      } yield ()
    } yield ()
    sb.toString
  }

  def saveData(): Try[Unit] = Try {
    val annotations = getAllAnnotations().filter(!_.question.isEmpty)
    val qaInfos = QAInfo.fromAnnotations(annotations)

    val aggByAssignment = AggregatedQAInfo.aggregateBy(
      List("hitType", "hitId", "assignmentId", "workerId", "acceptTime", "submitTime", "workerAssignmentNum"),
      qaInfo => List(qaInfo.annotation.hitType, qaInfo.annotation.hitId,
                     qaInfo.annotation.assignmentId, qaInfo.annotation.workerId,
                     qaInfo.annotation.acceptTime.toString, qaInfo.annotation.submitTime.toString,
                     qaInfo.assignmentNum.toString),
      qaInfos)
    val assignmentTSV = AggregatedQAInfo.makeTSV(aggByAssignment)
    FileManager.saveDataFile(experimentName, "assignments.tsv", assignmentTSV)

    val aggByHIT = AggregatedQAInfo.aggregateBy(
      List("hitType", "hitId"),
      qaInfo => List(qaInfo.annotation.hitType, qaInfo.annotation.hitId),
      qaInfos)
    val hitTSV = AggregatedQAInfo.makeTSV(aggByHIT)
    FileManager.saveDataFile(experimentName, "hits.tsv", hitTSV)

    val aggByWorker = AggregatedQAInfo.aggregateBy(
      List("hitType", "workerId"),
      qaInfo => List(qaInfo.annotation.hitType, qaInfo.annotation.workerId),
      qaInfos)
    val workerTSV = AggregatedQAInfo.makeTSV(aggByWorker)
    FileManager.saveDataFile(experimentName, "workers.tsv", workerTSV)

    // now for recording the most common words:

    def saveWordCounts[A <% String](filename: String, getWords: QAInfo => Iterable[A]) = {
      val allWords = for {
        qaInfo <- qaInfos
        word <- getWords(qaInfo)
      } yield word
      val wordCounts = counts(allWords)
      val tsv = "Word\tCount\n" + wordCounts.toVector.sortBy(-_._2).map {
        case (word, count) => s"$word\t$count"
      }.mkString("\n")
      FileManager.saveDataFile(experimentName, filename, tsv)
    }

    saveWordCounts("first-qword.tsv", qaInfo => List(qaInfo.questionFirstWord))
    saveWordCounts("first-qphrase.tsv", qaInfo => qaInfo.questionFirstPhrase.toList)
    saveWordCounts("first-qword-new.tsv", qaInfo => qaInfo.questionFirstWordIfNew.toList)
    saveWordCounts("new-qwords.tsv", _.newQuestionWords)
    saveWordCounts("new-awords.tsv", _.newAnswerWords)

    // all below is stats by HIT
    val proportionalLabelCoverage = for {
      aggInfo <- aggByHIT
      coveredLabel <- aggInfo.coveredArgLabels
    } yield ("Covered", coveredLabel)
    val proportionalLabelUncoverage = for {
      aggInfo <- aggByHIT
      uncoveredLabel <- aggInfo.uncoveredArgLabels
    } yield ("Uncovered", uncoveredLabel)
    val labelCoverageTSV = "IsCovered\tLabel\n" + (proportionalLabelCoverage ++ proportionalLabelUncoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(experimentName, "label-agg-coverage.tsv", labelCoverageTSV)

    val someWordLabelCoverage = for {
      aggInfo <- aggByHIT
      coveredLabel <- aggInfo.someWordCoveredLabels
    } yield ("Covered", coveredLabel)
    val noWordLabelCoverage = for {
      aggInfo <- aggByHIT
      uncoveredLabel <- aggInfo.noWordCoveredLabels
    } yield ("Uncovered", uncoveredLabel)
    val someWordLabelCoverageTSV = "IsCovered\tLabel\n" + (someWordLabelCoverage ++ noWordLabelCoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(experimentName, "label-some-word-coverage.tsv", someWordLabelCoverageTSV)

    val allWordsLabelCoverage = for {
      aggInfo <- aggByHIT
      coveredLabel <- aggInfo.allWordsCoveredLabels
    } yield ("Covered", coveredLabel)
    val someWordLabelUncoverage = for {
      aggInfo <- aggByHIT
      uncoveredLabel <- aggInfo.someWordUncoveredLabels
    } yield ("Uncovered", uncoveredLabel)
    val allWordsLabelCoverageTSV = "IsCovered\tLabel\n" + (allWordsLabelCoverage ++ someWordLabelUncoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(experimentName, "label-all-words-coverage.tsv", allWordsLabelCoverageTSV)

    val allArcMatches = aggByHIT.flatMap(_.arcMatchesForSome)
    val arcMatchesTSV = "DepLabel\tQuestionLabel\n" + allArcMatches
      .map { case (depLabel, qLabel) => s"$depLabel\t$qLabel" }
      .mkString("\n")
    FileManager.saveDataFile(experimentName, "arc-matches.tsv", arcMatchesTSV)
  }
}
