package example.emnlp2017.analysis

import example.emnlp2017.SentenceId
import example.emnlp2017.saveOutputFile

import cats.Show
import cats.implicits._

import turksem.qamr.QAData
import turksem.qamr.SourcedQA
import turksem.qamr.QAPairId

import turksem.util.pctString

import nlpdata.util.Text

class TemplateAnalysis[Slot : Show](
  label: String,
  data: QAData[SentenceId],
  templatizeAll: Map[QAPairId[SentenceId], SourcedQA[SentenceId]] => Map[QAPairId[SentenceId], QuestionTemplateAlignment[Slot]]) {
  val sqasById = data.all.map(sqa => sqa.id -> sqa).toMap
  val alignmentsById = templatizeAll(sqasById)
  val alignmentsBySentenceId = alignmentsById.groupBy(_._1.sentenceId)
  val alignmentsByTemplate = alignmentsById.map(_._2).groupBy(_.template)
  val templates = alignmentsByTemplate.keys.toList
  val templateCounts = alignmentsByTemplate.map(p => p._1 -> p._2.size)
  val templatesByFrequency = templateCounts.toVector.sortBy(-_._2)
  val proportionQAsCovered = alignmentsById.size.toDouble / sqasById.size
  val cumulativeCoverage = templatesByFrequency.scanLeft(0)(_ + _._2)

  def printAnalysis = {
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

    // TODO: histograms of templates by # occurrences, and QAs by # occurrences of template
    def occurringNTimes(n: Int) = templatesByFrequency.iterator.filter(_._2 == n).size
    println(s"Number of templates occuring once: ${occurringNTimes(1)}");
    (2 to 10).foreach { i =>
      println(s"Number of templates occuring $i times: ${occurringNTimes(i)}")
    }
  }

  // TODO: also create templating-pipeline-objects and have partial reporting of progress in templating everything

  def writeAllTSVs = {
    writeSentenceTSV
    writeSlotTSV
    writeFreqTSV
  }

  def writeSentenceTSV = {
    val sb = new StringBuilder
    data.sentenceToQAs.take(500).foreach {
      case (sid, sqas) =>
        val qtasForSentence = alignmentsBySentenceId.get(sid)
          .getOrElse(Map.empty[QAPairId[SentenceId], QuestionTemplateAlignment[Slot]])
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

  def writeSlotTSV = {
    val templateGroupsByType = alignmentsByTemplate.groupBy(_._1.toList)
    val sb = new StringBuilder
    templateGroupsByType.foreach {
      case (slots, templateGroup) =>
        val slotsRepr = if(slots.isEmpty) "<NONE>" else slots.map(_.show).mkString(", ")
        sb.append(slotsRepr + "\n")
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
