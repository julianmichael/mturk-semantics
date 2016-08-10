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
      // if sentence.words.map(_.token).filter(w =>
      //   !stopwords.contains(w.toLowerCase) && TextRendering.normalizeToken(w).exists(_.isLetter)
      // ).size >= 2
    } yield (CoNLLSentencePath(path, sentence.sentenceNum), sentence)
    allSentences.take(100).toList
  }

  // bucket sentences and create a task for each bucket
  def makeTask(system: ActorSystem)(minTokens: Int, maxTokens: Int, numQAs: Int, reward: Double) = {
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

  def start() = tasks.foreach(p => p._2 ! p._1.Message.Start)
  def stop() = tasks.foreach(p => p._2 ! p._1.Message.Stop)
  def disable() = tasks.foreach(p => p._2 ! p._1.Message.Disable)
  def expire() = tasks.foreach(p => p._2 ! p._1.Message.Expire)
  def update() = tasks.foreach(p => p._2 ! p._1.Message.Update)

  type QuestionData = (CoNLLSentencePath, String)
  type AnswerData = (List[(String, String)], String)

  def getAllAnnotations(): List[Annotation] =
    tasks.flatMap(p => FileManager.loadAnnotationsForHITType(p._1.hitType))

  def getAllQAPairs(): Map[String, (QuestionData, List[AnswerData])] =
    tasks.flatMap(_._1.annotatedQAPairs).toMap

  def getAllFeedback(): Iterable[String] = getAllQAPairs().flatMap(_._2._2.map(_._2))
    .filterNot(_.isEmpty)

  def saveData(): Try[Unit] = Try {
    FileManager.saveDataFile(name, getAllAnnotations(),
                             List(AnnotationStat.workerAssignmentNum))
  }
}
