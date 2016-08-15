package mts.experiments

import mts.core._
import mts.util._
import mts.tasks._
import mts.conll._
import akka.actor._

import scala.util.Try

// file name has A_ prepended so experiments have an obvious order
object OpenFormExperiment {
  val name = "a_open-form"
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

  // bucket sentences and create a task for each bucket
  private[this] def makeTask(system: ActorSystem)(minTokens: Int, maxTokens: Int, numQAs: Int, reward: Double) = {
    val taskSpec = OpenFormQATask(reward, numQAs, numAssignmentsPerHIT = 3)
    val filteredSentences = sentences.iterator
      .filter { case (_, sentence) => sentence.words.size >= minTokens && sentence.words.size <= maxTokens }
      .map { case (path, sentence) => (path, TextRendering.renderSentence(sentence)) }
    val actor = taskSpec.createMonitor(system, filteredSentences, 100)
    (taskSpec, actor)
  }

  lazy val system = ActorSystem("system")

  lazy val tasks = List[(OpenFormQATask, ActorRef)](
    makeTask(system)(minTokens = 7, maxTokens = 18, numQAs = 4, reward = 0.20),
    makeTask(system)(minTokens = 19, maxTokens = 120, numQAs = 6, reward = 0.30)
  )

  val protoTaskSpec = OpenFormQATask(0.0, 0, 0)
  val protoQASpec = protoTaskSpec.qaSpec

  def start() = tasks.foreach(p => p._2 ! p._1.Message.Start)
  def stop() = tasks.foreach(p => p._2 ! p._1.Message.Stop)
  def disable() = tasks.foreach(p => p._2 ! p._1.Message.Disable)
  def expire() = tasks.foreach(p => p._2 ! p._1.Message.Expire)
  def update() = tasks.foreach(p => p._2 ! p._1.Message.Update)

  type QuestionData = (CoNLLSentencePath, String)
  type AnswerData = (List[(String, String)], String)

  // TODO save HIT types and access them here and such
  def getAllAnnotations(): List[Annotation] =
    // tasks.flatMap(p => FileManager.loadAnnotationsForHITType(p._1.hitType))
    List("3NWM3X0LA7LBIGM2QP1Y4BFHQN9XPC", "3JH21YRKZ6B7ZFDTS077Y16U7HJ0JL")
      .flatMap(FileManager.loadAnnotationsForHITType)

  def getAllQAPairs(): Iterable[(QuestionData, List[AnswerData])] =
    tasks.flatMap(_._1.annotatedQAPairs).toMap.values

  def getAllFeedback(): Iterable[String] = getAllQAPairs().flatMap(_._2.map(_._2))
    .filterNot(_.isEmpty)

  def saveData(): Try[Unit] = Try {
    import mts.language._
    // q and a must be lower case
    case class QAInfo(
      val sentence: CoNLLSentence,
      val question: List[String],
      val answer: List[String]
    ) {
      val sentenceTokens = sentence.words.map(_.token)
      val indexedSentence = sentenceTokens.map(_.toLowerCase).zipWithIndex

      val sentenceSet = sentenceTokens.map(_.toLowerCase).toSet
      val questionSet = question.toSet
      val answerSet = answer.toSet

      val questionOverlap = indexedSentence.filter(p => questionSet(p._1)).map(_._2).toSet
      val proportionQuestionOverlap = questionOverlap.size.toDouble / sentenceTokens.size

      val answerOverlap = indexedSentence.filter(p => answerSet(p._1)).map(_._2).toSet
      val proportionAnswerOverlap = answerOverlap.size.toDouble / sentenceTokens.size

      val newQuestionWords = question.filterNot(sentenceSet)
      val newAnswerWords = answer.filterNot(sentenceSet)

      val questionFirstWord = question.head
      val questionFirstWordIfNew = if(!sentenceSet(questionFirstWord)) Some(question.head) else None

      val arcs = for {
        qIndex <- questionOverlap
        aIndex <- answerOverlap
        if qIndex != aIndex
      } yield (qIndex, aIndex)
    }

    // infos must be for the same sentence
    // this can be used both for per-assignment stats and per-sentence stats
    case class AggregatedQAInfo(
      val qas: List[QAInfo]
    ) {
      val sentence = qas.head.sentence

      val questionOverlap = qas.map(_.questionOverlap).reduce(_ union _)
      val questionOverlapCount = questionOverlap.size
      val questionOverlapPerQA = questionOverlapCount.toDouble / qas.size
      val questionOverlapProportion = questionOverlap.size.toDouble / sentence.words.size

      val answerOverlap = qas.map(_.answerOverlap).reduce(_ union _)
      val answerOverlapCount = answerOverlap.size
      val answerOverlapPerQA = answerOverlapCount.toDouble / qas.size
      val answerOverlapProportion  = answerOverlap.size.toDouble / sentence.words.size

      val arcs = qas.map(_.arcs).reduce(_ union _)

      val coveredArgLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        spanWord <- words
        if arcs.contains(pred.head.index, spanWord.index) && !label.equals("V")
      } yield label
      val uncoveredArgLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        spanWord <- words
        if !arcs.contains(pred.head.index, spanWord.index) && !label.equals("V")
      } yield label
      val coveredArgLabelCount = coveredArgLabels.size
      val uncoveredArgLabelCount = uncoveredArgLabels.size
      val coveredLabelProportion = coveredArgLabels.size.toDouble / (coveredArgLabels.size + uncoveredArgLabels.size)

      val someWordCoveredLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        if !label.equals("V") && words.exists(spanWord => arcs.contains(pred.head.index, spanWord.index))
      } yield label
      val noWordCoveredLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        if !label.equals("V") && !words.exists(spanWord => arcs.contains(pred.head.index, spanWord.index))
      } yield label
      val someWordCoveredLabelCount = someWordCoveredLabels.size
      val noWordCoveredLabelCount = noWordCoveredLabels.size
      val someWordCoveredLabelProportion = someWordCoveredLabelCount.toDouble / (someWordCoveredLabelCount + noWordCoveredLabelCount)

      val allWordsCoveredLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        if !label.equals("V") && words.forall(spanWord => arcs.contains(pred.head.index, spanWord.index))
      } yield label
      val someWordUncoveredLabels = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(label, words) <- args
        if !label.equals("V") && !words.forall(spanWord => arcs.contains(pred.head.index, spanWord.index))
      } yield label
      val allWordsCoveredLabelCount = allWordsCoveredLabels.size
      val someWordUncoveredLabelCount = someWordUncoveredLabels.size
      val allWordsCoveredLabelProportion = allWordsCoveredLabelCount.toDouble / (allWordsCoveredLabelCount + someWordUncoveredLabelCount)

      val arcMatches = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(depLabel, argWords) <- args
        if !depLabel.equals("V")
        qaInfo <- qas
        questionLabel <- qaInfo.questionFirstWordIfNew.toList
        word <- argWords
        if qaInfo.arcs.contains(pred.head.index, word.index)
      } yield (depLabel, questionLabel)

      val arcMatchesForSome = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(depLabel, argWords) <- args
        if !depLabel.equals("V")
        qaInfo <- qas
        questionLabel <- qaInfo.questionFirstWordIfNew.toList
        if argWords.exists(word => qaInfo.arcs.contains(pred.head.index, word.index))
      } yield (depLabel, questionLabel)

      val arcMatchesForAll = for {
        PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
        ArgumentSpan(depLabel, argWords) <- args
        if !depLabel.equals("V")
        qaInfo <- qas
        questionLabel <- qaInfo.questionFirstWordIfNew.toList
        if argWords.exists(word => qaInfo.arcs.contains(pred.head.index, word.index))
      } yield (depLabel, questionLabel)
    }

    // assume we have the queston for everything ugh
    def aggregatedInfoForAnnotation(a: Annotation) = {
      val question = a.question.get
      val ((path, _), (qaPairs, _)) = (protoQASpec.extractQuestionData(question), protoQASpec.extractAnswerData(a.answer))
      val infos = for {
        sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
        (q, a) <- qaPairs
        qTokens = tokenize(q).map(_.toLowerCase)
        aTokens = tokenize(a).map(_.toLowerCase)
      } yield QAInfo(sentence, qTokens, aTokens)
      AggregatedQAInfo(infos.toList)
    }

    val annotations = getAllAnnotations().filter(!_.question.isEmpty)
    val annotationsToInfos = annotations.map(a => a -> aggregatedInfoForAnnotation(a)).toMap

    // now calculate some stats
    val questionOverlapCountStat = AnnotationStat("questionOverlapCount", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).questionOverlapCount}%.4f"))

    val questionOverlapProportionStat = AnnotationStat("questionOverlapProportion", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).questionOverlapProportion}%.4f"))

    val questionOverlapPerQAStat = AnnotationStat("questionOverlapPerQA", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).questionOverlapPerQA}%.4f"))

    val answerOverlapCountStat = AnnotationStat("answerOverlapCount", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).answerOverlapCount}%.4f"))

    val answerOverlapProportionStat = AnnotationStat("answerOverlapProportion", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).answerOverlapProportion}%.4f"))

    val answerOverlapPerQAStat = AnnotationStat("answerOverlapPerQA", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).answerOverlapPerQA}%.4f"))

    val coveredLabelProportionStat = AnnotationStat("coveredLabelProportion", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).coveredLabelProportion}%.4f"))

    val someWordCoveredLabelProportionStat = AnnotationStat("someWordCoveredLabelProportion", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).someWordCoveredLabelProportion}%.4f"))

    val allWordsCoveredLabelProportionStat = AnnotationStat("allWordsCoveredLabelProportion", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).allWordsCoveredLabelProportion}%.4f"))

    val assignmentFileContents = Annotation.toTSV(annotations,
                                                  List(AnnotationStat.workerAssignmentNum,
                                                       questionOverlapCountStat,
                                                       questionOverlapProportionStat,
                                                       questionOverlapPerQAStat,
                                                       answerOverlapCountStat,
                                                       answerOverlapPerQAStat,
                                                       answerOverlapProportionStat,
                                                       coveredLabelProportionStat,
                                                       someWordCoveredLabelProportionStat,
                                                       allWordsCoveredLabelProportionStat
                                                  ))
    FileManager.saveDataFile(name, "assignments.tsv", assignmentFileContents)

    // now let's compute these stats by HIT instead of just assignment

    val infoAggregatedByHIT = annotations.groupBy(_.hitId).map {
      case (hitId, annos) => hitId -> (annos.head.hitType, AggregatedQAInfo(annos.flatMap(a => aggregatedInfoForAnnotation(a).qas)))
    }
    val hitTSV = "hitId\thitType\tquestionOverlapCount\tquestionOverlapProportion\tquestionOverlapPerQA\t" +
      "answerOverlapCount\tanswerOverlapProportion\tanswerOverlapPerQA\tcoveredLabelProportion\tsomeWordCoveredLabelProportion\tallWordsCoveredLabelProportion\n" +
      infoAggregatedByHIT.toList.map { case (hitId, (hitType, aggInfo)) =>
        import aggInfo._
        s"$hitId\t$hitType\t$questionOverlapCount\t$questionOverlapProportion\t$questionOverlapPerQA\t" +
          s"$answerOverlapCount\t$answerOverlapProportion\t$answerOverlapPerQA\t$coveredLabelProportion\t$someWordCoveredLabelProportion\t$allWordsCoveredLabelProportion"
      }.mkString("\n")
    FileManager.saveDataFile(name, "hits.tsv", hitTSV)

    // now for recording the most common words:

    def saveWordCounts(filename: String, getWords: QAInfo => Iterable[String]) = {
      val allWords = for {
        aggInfo <- annotationsToInfos.values
        qaInfo <- aggInfo.qas
        word <- getWords(qaInfo)
      } yield word
      val wordCounts = counts(allWords)
      val tsv = "Word\tCount\n" + wordCounts.toVector.sortBy(-_._2).map {
        case (word, count) => s"$word\t$count"
      }.mkString("\n")
      FileManager.saveDataFile(name, filename, tsv)
    }

    saveWordCounts("first-qword.tsv", qaInfo => List(qaInfo.questionFirstWord))
    saveWordCounts("first-qword-new.tsv", qaInfo => qaInfo.questionFirstWordIfNew.toList)
    saveWordCounts("new-qwords.tsv", _.newQuestionWords)
    saveWordCounts("new-awords.tsv", _.newAnswerWords)

    // all below is stats by HIT
    val proportionalLabelCoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      coveredLabel <- aggInfo.coveredArgLabels
    } yield ("Covered", coveredLabel)
    val proportionalLabelUncoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      uncoveredLabel <- aggInfo.uncoveredArgLabels
    } yield ("Uncovered", uncoveredLabel)
    val labelCoverageTSV = "IsCovered\tLabel\n" + (proportionalLabelCoverage ++ proportionalLabelUncoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(name, "label-agg-coverage.tsv", labelCoverageTSV)

    val someWordLabelCoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      coveredLabel <- aggInfo.someWordCoveredLabels
    } yield ("Covered", coveredLabel)
    val noWordLabelCoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      uncoveredLabel <- aggInfo.noWordCoveredLabels
    } yield ("Uncovered", uncoveredLabel)
    val someWordLabelCoverageTSV = "IsCovered\tLabel\n" + (someWordLabelCoverage ++ noWordLabelCoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(name, "label-some-word-coverage.tsv", someWordLabelCoverageTSV)

    val allWordsLabelCoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      coveredLabel <- aggInfo.allWordsCoveredLabels
    } yield ("Covered", coveredLabel)
    val someWordLabelUncoverage = for {
      (_, aggInfo) <- infoAggregatedByHIT.values
      uncoveredLabel <- aggInfo.someWordUncoveredLabels
    } yield ("Uncovered", uncoveredLabel)
    val allWordsLabelCoverageTSV = "IsCovered\tLabel\n" + (allWordsLabelCoverage ++ someWordLabelUncoverage)
      .map(p => s"${p._1}\t${p._2}")
      .mkString("\n")
    FileManager.saveDataFile(name, "label-all-words-coverage.tsv", allWordsLabelCoverageTSV)

    val allArcMatches = infoAggregatedByHIT.values.flatMap(_._2.arcMatchesForSome)
    val arcMatchesTSV = "DepLabel\tQuestionLabel\n" + allArcMatches
      .map { case (depLabel, qLabel) => s"$depLabel\t$qLabel" }
      .mkString("\n")
    FileManager.saveDataFile(name, "arc-matches.tsv", arcMatchesTSV)
  }
}
