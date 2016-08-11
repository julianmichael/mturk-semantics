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

  // def quit() = {
  //   system.terminate()
  //   System.exit(0)
  // }

  type QuestionData = (CoNLLSentencePath, String)
  type AnswerData = (List[(String, String)], String)

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
      val sentence: List[String],
      val question: List[String],
      val answer: List[String]
    ) {
      val indexedSentence = sentence.map(_.toLowerCase).zipWithIndex

      val sentenceSet = sentence.map(_.toLowerCase).toSet
      val questionSet = question.toSet
      val answerSet = answer.toSet

      val questionOverlap = indexedSentence.filter(p => questionSet(p._1)).map(_._2).toSet
      val proportionQuestionOverlap = questionOverlap.size.toDouble / sentence.size

      val answerOverlap = indexedSentence.filter(p => answerSet(p._1)).map(_._2).toSet
      val proportionAnswerOverlap = answerOverlap.size.toDouble / sentence.size

      val newQuestionWords = question.filterNot(sentenceSet)
      val newAnswerWords = answer.filterNot(sentenceSet)

      val arcs = for {
        qIndex <- questionOverlap
        aIndex <- answerOverlap
        if qIndex != aIndex
      } yield (qIndex, aIndex)
      val arcCoverage = arcs.size
      // theoretical max of n(n - 1) arcs between different words
      val arcCoverageOfN2 = arcs.size.toDouble / (sentence.size * (sentence.size - 1))
      // practical max of n - 1 arcs forming a tree
      val arcCoverageOfN1 = arcs.size.toDouble / (sentence.size - 1)
      // in reality we will get numbers in between these, counting spurious arcs from function words, etc.

      val questionFirstWord = question.head
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
      val questionOverlapProportion = questionOverlap.size.toDouble / sentence.size

      val answerOverlap = qas.map(_.answerOverlap).reduce(_ union _)
      val answerOverlapCount = answerOverlap.size
      val answerOverlapPerQA = answerOverlapCount.toDouble / qas.size
      val answerOverlapProportion  = answerOverlap.size.toDouble / sentence.size

      val arcs = qas.map(_.arcs).reduce(_ union _)
      val arcCoverage = arcs.size
      val arcCoverageOfN2 = arcs.size.toDouble / (sentence.size * (sentence.size - 1))
      val arcCoverageOfN1 = arcs.size.toDouble / (sentence.size - 1)
    }

    // assume we have the queston for everything ugh
    def aggregatedInfoForAnnotation(a: Annotation) = {
      val question = a.question.get
      val ((path, _), (qaPairs, _)) = (protoQASpec.extractQuestionData(question), protoQASpec.extractAnswerData(a.answer))
      val infos = for {
        sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
        tokens = sentence.words.map(_.token)
        (q, a) <- qaPairs
        qTokens = tokenize(q).map(_.toLowerCase)
        aTokens = tokenize(a).map(_.toLowerCase)
      } yield QAInfo(tokens, qTokens, aTokens)
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

    val arcCoverageStat = AnnotationStat("arcCoverage", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).arcCoverage}%.4f"))

    val arcCoverageN1Stat = AnnotationStat("arcCoverageN1", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).arcCoverageOfN1}%.4f"))

    val arcCoverageN2Stat = AnnotationStat("arcCoverageN2", (annos: List[Annotation]) =>
      annos.map(a => f"${annotationsToInfos(a).arcCoverageOfN2}%.4f"))

    val assignmentFileContents = Annotation.toTSV(annotations,
                                                  List(AnnotationStat.workerAssignmentNum,
                                                       questionOverlapCountStat,
                                                       questionOverlapProportionStat,
                                                       questionOverlapPerQAStat,
                                                       answerOverlapCountStat,
                                                       answerOverlapPerQAStat,
                                                       answerOverlapProportionStat,
                                                       arcCoverageStat,
                                                       arcCoverageN1Stat,
                                                       arcCoverageN2Stat))
    FileManager.saveDataFile(name, "assignments.tsv", assignmentFileContents)

    // now let's compute these stats by HIT instead of just assignment

    val infoAggregatedByHIT = annotations.groupBy(_.hitId).map {
      case (hitId, annos) => hitId -> (annos.head.hitType, AggregatedQAInfo(annos.flatMap(a => aggregatedInfoForAnnotation(a).qas)))
    }
    val hitTSV = "hitId\thitType\tquestionOverlapCount\tquestionOverlapProportion\tquestionOverlapPerQA\t" +
      "answerOverlapCount\tanswerOverlapProportion\tanswerOverlapPerQA\tarcCoverage\tarcCoverageN1\tarcCoverageN2\n" +
      infoAggregatedByHIT.toList.map { case (hitId, (hitType, aggInfo)) =>
        import aggInfo._
        s"$hitId\t$hitType\t$questionOverlapCount\t$questionOverlapProportion\t$questionOverlapPerQA\t" +
          s"$answerOverlapCount\t$answerOverlapProportion\t$answerOverlapPerQA\t$arcCoverage\t$arcCoverageOfN1\t$arcCoverageOfN2"
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
    saveWordCounts("new-qwords.tsv", _.newQuestionWords)
    saveWordCounts("new-awords.tsv", _.newAnswerWords)
  }
}
