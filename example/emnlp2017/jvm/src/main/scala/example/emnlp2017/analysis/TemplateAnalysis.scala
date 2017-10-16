package example.emnlp2017.analysis

import example.emnlp2017.SentenceId
import example.emnlp2017.saveOutputFile

import cats.Show
import cats.Order
import cats.implicits._

import turksem.qamr.QAData
import turksem.qamr.SourcedQA
import turksem.qamr.QAPairId

import turksem.util._

import nlpdata.util.Text
import nlpdata.util.PosTags
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.WiktionaryFileSystemService

object TemplateAnalysis {
  import TemplatingPhase._

  // TODO fold in genitive clitics too? nah
  val fullPos = posPhase andThen filterAdjAndAdvPhase
  def postprocess(phase: TemplatingPhase) =
    phase andThen /* collapseContigProperNounsPhase andThen */ deleteRedundantDeterminersPhase

  val fullAbstractivePipeline =
    postprocess(abstractVerbsPhase) ::
      postprocess(abstractSimpleNounsPhase) ::
      postprocess(abstractNounsPhase) ::
      postprocess(abstractAdjectivesPhase) ::
      postprocess(abstractNumbersPhase) ::
      postprocess(abstractExpletiveTherePhase) ::
      postprocess(abstractPossessivePronounsPhase) ::
      postprocess(abstractAdverbsPhase) ::
      postprocess(abstractPrepositionsPhase) ::
      Nil

  lazy val defaultPipeline = List(
    List(fullPos, oneWordOnlyPhase), fullAbstractivePipeline, List(dropPOSPhase),
    List(fullPos), fullAbstractivePipeline,
    List(generalizePlaceholderObjectsPhase, collapseProperAndPluralNounsPhase,
         foldDeterminersPhase, dropPOSPhase)
  ).flatten

  def getDefaultAnalysis(label: String, data: QAData[SentenceId]) =
    new TemplateAnalysis(label, data, defaultPipeline)
}

// NOTE might generalize slot type in the future, but probably not
class TemplateAnalysis(
  label: String,
  data: QAData[SentenceId],
  templatingPipeline: List[TemplatingPhase]) {

  val Wiktionary = new nlpdata.datasets.wiktionary.WiktionaryFileSystemService(
    java.nio.file.Paths.get("resources/wiktionary")
  )

  implicit lazy val inflections = {
    val tokens = for {
      id <- data.sentenceToQAs.keys.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  val sqasById = data.all.map(sqa => sqa.id -> sqa).toMap

  val namedTemplatingResults = templatingPipeline
    .scanLeft(("start", TemplatingResult.initFromQuestions(sqasById))) {
    case (acc, phase) => (phase.name, phase.run(acc._2))
  }
  val templatingResult = namedTemplatingResults.last._2

  val alignmentsById = templatingResult.resultAlignments
  val alignmentsBySentenceId = alignmentsById.groupBy(_._1.sentenceId)
  val alignmentsByTemplate = alignmentsById.map(_._2).groupBy(_.template)
  val templates = alignmentsByTemplate.keys.toList
  val templateCounts = alignmentsByTemplate.map(p => p._1 -> p._2.size)
  val templatesByFrequency = templateCounts.toVector.sortBy(-_._2)
  val proportionQAsCovered = alignmentsById.size.toDouble / sqasById.size
  val cumulativeCoverage = templatesByFrequency.scanLeft(0)(_ + _._2)

  // TODO LIST:
  // - change back to letting <obj> appear in templates, in fact forget about folding in the more general ones for now
  // - stop folding determiners into nouns
  // - redo paraphrasing in the stricter way with alignment
  // - add mirroring in addition to paraphrasing: each set corresponds to a "role", we can find it with the right trigger
  // - make both paraphrasing and mirroring DIRECTIONAL (i.e. look like entailment)? regardless,
  //   - mirroring should either not be transitive or not be symmetric, as a starting point.
  // - for cases of mirroring, identify which side is the more useful trigger
  // - change templating logic to replace all parts of speech as we see fit... maybe

  // TODO redo paraphrasing in this much stricter way
  // case class ParaphrasedTemplateAlignment(
  //   template1: QuestionTemplateAlignment[TriggerSlot],
  //   template2: QuestionTemplateAlignment[TriggerSlot],
  //   permutation: List[Int] // only permutes question alignments
  // )
  // object ParaphrasedTemplateAlignment {
  //   def getPossibleAlignments(
  //     x: QuestionTemplateAlignment[TriggerSlot],
  //     y: QuestionTemplateAlignment[TriggerSlot]
  //   ): List[ParaphrasedTemplateAlignment] = {
  //     val alignmentSizesMatch = x.alignments.size != y.alignments.size
  //     val xTriggerLabel = getTriggerSlot(x).label.takeWhile(_ != '-')
  //     val yTriggerLabel = getTriggerSlot(y).label.takeWhile(_ != '-')
  //     val triggersMatch = xTriggerLabel == yTriggerLabel
  //     if( || ) Nil
  //     else {

  //     }
  //   }
  // }

  def computeSingleWordAnswerSimilarity(
    x: QuestionTemplateAlignment[TriggerSlot],
    y: QuestionTemplateAlignment[TriggerSlot]
  ): Option[Double] = {
    val xAnswerIndices = x.sourcedQA.answers.filter(_.size == 1).map(_.head).toSet
    val yAnswerIndices = y.sourcedQA.answers.filter(_.size == 1).map(_.head).toSet
    val isAdmissible = xAnswerIndices.nonEmpty && yAnswerIndices.nonEmpty
    val isGood = xAnswerIndices.exists(yAnswerIndices.contains)
    if(isAdmissible) {
      Some(if(isGood) 1.0 else 0.0)
    } else None
  }

  def computeSetwiseAnswerSimilarity(
    x: QuestionTemplateAlignment[TriggerSlot],
    y: QuestionTemplateAlignment[TriggerSlot]
  ): Option[Double] = {
    val xAnswerIndices = x.sourcedQA.answers.filter(_.size == 1).map(_.head).toSet
    val yAnswerIndices = y.sourcedQA.answers.filter(_.size == 1).map(_.head).toSet
    val isAdmissible = xAnswerIndices.nonEmpty && yAnswerIndices.nonEmpty
    val isGood = xAnswerIndices.exists(yAnswerIndices.contains)
    Some(
      x.sourcedQA.answers.flatMap(xa =>
        y.sourcedQA.answers.map(ya =>
          SetMetric.intersectionOverUnion(xa, ya)
        )
      ).max
    )
  }

  case class AggregateSimilarity(
    template1: QuestionTemplate[TriggerSlot],
    template2: QuestionTemplate[TriggerSlot],
    totalScore: Double,
    numInstances: Int) {
    def meanScore = totalScore / numInstances
  }
  object AggregateSimilarity {
    implicit val aggregateSimilarityOrder = Order.by[AggregateSimilarity, (Double, Int)](s => s.meanScore -> s.numInstances).reverse
    implicit val aggregateSimilarityOrdering = aggregateSimilarityOrder.toOrdering
    implicit val aggregateSimilarityShow = new Show[AggregateSimilarity] {
      def show(as: AggregateSimilarity) =
        as.template1.show + "\t" + as.template2.show + "\t" +
          f"${as.meanScore}%.2f" + "\t" + as.numInstances
    }
  }


  def similarities(
    getSimilarityInstanceOpt: (
      QuestionTemplateAlignment[TriggerSlot],
      QuestionTemplateAlignment[TriggerSlot]
    ) => Option[Double]
  ) = alignmentsBySentenceId.iterator.flatMap { case (id, qtasForSentence) =>
    qtasForSentence
      .map(_._2)
      .filter(qta => templateCounts(qta.template) > 2)
      .groupBy(qta => getTriggerWordStem(qta))
      .flatMap { case (triggerStem, qtasForWord) =>
        def getAllPairwiseScores(
          qtas: List[QuestionTemplateAlignment[TriggerSlot]]
        ): List[(QuestionTemplate[TriggerSlot], QuestionTemplate[TriggerSlot], Double)] = qtas match {
          case Nil => Nil
          case x :: tail =>
            tail.flatMap { y =>
              getSimilarityInstanceOpt(x, y).map((x.template min y.template, x.template max y.template, _))
            } ++ getAllPairwiseScores(tail)
        }
        getAllPairwiseScores(qtasForWord.toList)
    }
  }.toVector.groupBy(tuple => (tuple._1, tuple._2)).iterator.collect {
    case ((template1, template2), similarities)
        if template1 != template2 && templateCounts(template1) > 2 && templateCounts(template2) > 2 =>
      AggregateSimilarity(template1, template2, similarities.map(_._3).toList.sum, similarities.size)
  }.toVector.sorted

  def templateClusters(
    chosenSimilarities: Vector[AggregateSimilarity] = similarities(computeSetwiseAnswerSimilarity),
    numInstancesThreshold: Int = 4,
    accuracyThreshold: Double = 0.9
  ) = {
    implicit val qtOrdering = QuestionTemplate.questionTemplateOrder[TriggerSlot].toOrdering
    var updatedSimilarities = chosenSimilarities.toList
    val clusters = MutableUnionFind.empty[QuestionTemplate[TriggerSlot]]
    updatedSimilarities.foreach {
      case AggregateSimilarity(t1, t2, _, _) =>
        clusters.add(t1)
        clusters.add(t2)
    }
    while(updatedSimilarities.filter(_.numInstances >= numInstancesThreshold).headOption.fold(false)(s => s.meanScore >= accuracyThreshold)) {
      val nextIndex = updatedSimilarities.findIndex(_.numInstances >= numInstancesThreshold).get
      val AggregateSimilarity(t1, t2, totalScore, numInstances) = updatedSimilarities(nextIndex)
      println(f"UNITING: ${t1.show}%-50s ${t2.show}%-50s ${totalScore / numInstances}%5.2f $numInstances%d")
      updatedSimilarities = updatedSimilarities.take(nextIndex) ++ updatedSimilarities.drop(nextIndex + 1)
      clusters.union(t1, t2)
      val newRep = clusters.find(t1).get
      val sortedChangedSimilarities = updatedSimilarities.collect {
        case AggregateSimilarity(x, y, totalScore, numInstances) if x == t1 || x == t2 || y == t1 || y == t2 =>
          val other = if(x == t1 || x == t2) y else x
          (other, totalScore, numInstances)
      }.groupBy(_._1).toList.map { case (other, tuples) =>
          AggregateSimilarity(newRep min other, newRep max other, tuples.map(_._2).sum, tuples.map(_._3).sum)
      }.sorted
      updatedSimilarities = updatedSimilarities.filterNot {
        case AggregateSimilarity(x, y, _, _) => x == t1 || x == t2 || y == t1 || y == t2
      }
      updatedSimilarities = mergeSortedLists(sortedChangedSimilarities, updatedSimilarities)
    }
    (clusters, updatedSimilarities)
  }

  // case class CrossTemplateAlignment(
  //   source: QuestionTemplateAlignment[TriggerSlot],
  //   target: QuestionTemplateAlignment[TriggerSlot],
  //   permutation: List[Int]) {
  //   // def isParaphrasePossible =
  // }

  lazy val interestingQTAPairs = {
    alignmentsBySentenceId.iterator.flatMap { case (id, qtasForSentence) =>
      qtasForSentence.flatMap { case (_, qta) =>
        val singleWordAnswerIndices = qta.sourcedQA.answers.filter(_.size == 1).map(_.head)
        val singleWordQuestionAlignments = qta.alignments.map(_.filter(_.indices.size == 1).map(_.begin))
        val allQAAlignmentSets = (singleWordAnswerIndices :: singleWordQuestionAlignments)
          .filter(_.nonEmpty).sequence.map(_.toSet)
        for {
          (_, otherQTA) <- qtasForSentence
          if otherQTA != qta
          result <- {
            val otherQTAAlignmentSets = for {
              otherAnswerIndex <- otherQTA.sourcedQA.answers.filter(_.size == 1).map(_.head)
              otherQuestionAlignment <- otherQTA.alignments.map(_.filter(_.indices.size == 1).map(_.begin)).filter(_.nonEmpty).sequence
            } yield otherQuestionAlignment.toSet + otherAnswerIndex
            if(allQAAlignmentSets.exists(aligns => otherQTAAlignmentSets.exists(_.subsetOf(aligns)))) {
              Some(otherQTA -> qta)
            } else None
          }
        } yield result
      }
    }.toVector
  }

  lazy val interestingTemplatePairCounts = interestingQTAPairs
    .groupBy { case (qta1, qta2) => (qta1.template, qta2.template) }
    .toVector.sortBy(-_._2.size)

  def printAnalysis = {
    println(s"Intermediate results:")
    namedTemplatingResults.foreach { case (name, result) =>
      println
      println(s"Phase: ${name.capitalize}")
      println(f"Coverage: ${result.proportionQAsCovered}%.2f")
      println(f"Turnover: ${result.proportionQAsWithNewTemplate}%.2f")

      def printFrequentExampleAlignments(
        alignments: Map[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]]
      ) = alignments.values
        .groupBy(_.template).toVector
        .sortBy(-_._2.size)
        .take(10)
        .foreach { case (template, qtas) =>
          val sqa = qtas.head.sourcedQA
          val sid = sqa.id.sentenceId
          val sentence = Text.render(sid)
          val question = sqa.wqa.question
          val answer = Text.renderSpan(sid, sqa.wqa.answer)
          println(f"${template.show}%-30s ${qtas.size}%4d $question%-30s $answer%30s")
      }

      println
      println(s"Most common new alignment templates:")
      printFrequentExampleAlignments(result.newQuestionAlignments)
      println
      println(s"Most common lost alignment templates:")
      printFrequentExampleAlignments(result.lostQuestionAlignments)

      println
      println(s"Most common new templates:")
      printFrequentExampleAlignments(result.newTemplateAlignments)
      println
      println(s"Most common lost templates:")
      printFrequentExampleAlignments(result.lostTemplateAlignments)

      println
    }

    println(s"Number of QAs: ${data.all.size}")
    println(s"Number of QAs covered: ${pctString(alignmentsById.size, sqasById.size)} ")
    println(s"Number of unique templates: ${templateCounts.size}")

    println(s"Most common templates: ")
    templatesByFrequency.take(10).foreach { case (template, count) =>
      println(f"${template.show}%-52s$count%d")
    }
    println(s"Least common templates: ")
    templatesByFrequency.takeRight(10).reverse.foreach { case (template, count) =>
      println(f"${template.show}%-52s$count%d")
    }

    println("Cumulative coverage:")
    val coveragesToReport = (0.0 to proportionQAsCovered by 0.05)
    def reportCoverage(pct: Double) = {
      val numTemplatesForPct = cumulativeCoverage.takeWhile(_ < (data.all.size * pct)).size
      println(f"${(pct * 100).toInt}%d%% coverage with $numTemplatesForPct%d templates")
    }
    coveragesToReport.foreach(reportCoverage)
    reportCoverage(proportionQAsCovered)

    def occurringNTimes(n: Int) = templatesByFrequency.iterator.filter(_._2 == n).size
    println(s"Number of templates occuring once: ${occurringNTimes(1)}");
    (2 to 10).foreach { i =>
      println(s"Number of templates occuring $i times: ${occurringNTimes(i)}")
    }
  }

  def writeAllTSVs = {
    writeSentenceTSV
    writeSlotTSV
    writeFreqTSV
    writeWordTSV
    writeClusterTSV
  }

  def getTriggerSlotIndex(template: QuestionTemplate[TriggerSlot]): Int = {
    template.toList.findIndex(_.isTrigger).get
  }

  def getTriggerSlot(template: QuestionTemplate[TriggerSlot]): TriggerSlot = {
    template.find(_.isTrigger).get
  }

  def getTriggerSpan(qta: QuestionTemplateAlignment[TriggerSlot]): Option[ContiguousSpan] = {
    qta.alignments(getTriggerSlotIndex(qta.template)).headOption
  }

  def getTriggerWordStem(qta: QuestionTemplateAlignment[TriggerSlot]): String = {
    getTriggerSpan(qta).fold(getTriggerSlot(qta.template).label) { contigSpan =>
      val spanStr = Text.renderSpan(qta.sourcedQA.id.sentenceId, contigSpan.indices).lowerCase
      inflections.getInflectedForms(spanStr).fold(spanStr)(_.stem).toString
    }
  }

  def writeWordTSV = {
    val sb = new StringBuilder
    val qtasByWord = templatingResult.resultAlignments.values.toList.groupBy(getTriggerWordStem)
    val qtasSortedByTriggerFreq = qtasByWord.toVector.sortBy(-_._2.size)
    qtasSortedByTriggerFreq.take(500).foreach { case (word, qtasForWord) =>
      sb.append(word)
      sb.append("\t")
      val qtasSortedByTemplateFreq = qtasForWord.groupBy(_.template).toVector.sortBy(-_._2.size)
      qtasSortedByTemplateFreq.foreach { case (template, qtasForTemplate) =>
        sb.append(template.show + "\t" + qtasForTemplate.size + "\t")
        val qta = qtasForTemplate.head
        val sid = qta.sourcedQA.id.sentenceId
        sb.append(qta.sourcedQA.question + "\t" + Text.renderSpan(sid, qta.sourcedQA.wqa.answer) + "\t")
        sb.append(Text.render(sid) + "\n\t")
      }
      sb.append("\n")
    }
    saveOutputFile(s"$label-templates-word.tsv", sb.toString)
  }

  def writeSentenceTSV = {
    val sb = new StringBuilder
    data.sentenceToQAs.take(500).foreach {
      case (sid, sqas) =>
        val qtasForSentence = alignmentsBySentenceId.get(sid)
          .getOrElse(Map.empty[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]])
        sb.append(SentenceId.toString(sid) + "\t" + Text.render(sid) + "\n")
        sqas.foreach { sqa =>
          sb.append("\t")
          sb.append(qtasForSentence.get(sqa.id).fold("")(_.template.show) + "\t")
          sb.append(sqa.question + "\t")
          sb.append(Text.renderSpan(sid, sqa.wqa.answer) + "\n")
        }
    }
    saveOutputFile(s"$label-templates-sentence.tsv", sb.toString)
  }

  def writeClusterTSV = {
    val (clusters, _) = templateClusters()
    val templateToRep = clusters.iterator.map { t =>
      t -> clusters.find(t).getOrElse(t)
    }.toMap.withDefault(identity)

    val templateClusterByRep = alignmentsByTemplate
      .filter(p => templateCounts(p._1) > 1)
      .groupBy(p => templateToRep(p._1))
    val templateClustersByTrigger = templateClusterByRep.groupBy(p => getTriggerSlot(p._1))
    val sb = new StringBuilder
    templateClustersByTrigger.foreach {
      case (triggerSlot, clustersByRep) =>
        sb.append(triggerSlot.label + "\n")
        clustersByRep.toVector.sortBy(-_._2.map(_._2.size).sum).foreach {
          case (rep, cluster) =>
            val bestRep = cluster.maxBy(_._2.size)._1
            val clusterSize = cluster.map(_._2.size).sum
            if(clusterSize >= 1) {
              sb.append(s"\t${bestRep.show}\t${clusterSize}\t")
              cluster.toVector.sortBy(-_._2.size).foreach { case (template, qtas) =>
                val qta = qtas.head
                val id = qta.sourcedQA.id.sentenceId
                sb.append(s"${template.show}\t${qtas.size}\t")
                sb.append(s"${qta.sourcedQA.question}\t${Text.renderSpan(id, qta.sourcedQA.wqa.answer)}\t${Text.render(id)}\n\t\t\t")
              }
              sb.append("\n")
            }
        }
        sb.append("\n")
    }
    saveOutputFile(s"$label-templates-clusters.tsv", sb.toString)
  }

  def writeSlotTSV = {
    val templateGroupsByType = alignmentsByTemplate.groupBy(p => getTriggerSlot(p._1))
    val sb = new StringBuilder
    templateGroupsByType.foreach {
      case (triggerSlot, templateGroup) =>
        sb.append(triggerSlot.label + "\n")
        templateGroup.toVector.sortBy(-_._2.size).foreach {
          case (template, qtas) =>
            if(qtas.size > 1) {
              val qta = qtas.head
              val id = qta.sourcedQA.id.sentenceId
              sb.append(s"\t${template.show}\t${qtas.size}\t")
              sb.append(s"${qta.sourcedQA.question}\t${Text.renderSpan(id, qta.sourcedQA.wqa.answer)}\t${Text.render(id)}\n")
            }
        }
        sb.append("\n")
    }
    saveOutputFile(s"$label-templates-slots.tsv", sb.toString)
  }

  def writeFreqTSV = {
    var sb = new StringBuilder
    alignmentsByTemplate.toVector.sortBy(-_._2.size).foreach {
      case (template, qtas) if qtas.size > 1 =>
        sb.append(s"${template.show}\t${qtas.size}\n")
        qtas.take(10).foreach { qta =>
          val id = qta.sourcedQA.id.sentenceId
          sb.append(s"\t${qta.sourcedQA.question}\t${Text.renderSpan(id, qta.sourcedQA.wqa.answer)}\t${Text.render(id)}\n")
        }
        sb.append("\n")
      case _ => ()
    }
    saveOutputFile(s"$label-templates-freq.tsv", sb.toString)
  }

}
