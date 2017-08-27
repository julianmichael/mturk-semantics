package example.emnlp2017

import turksem._
import example.emnlp2017._
import turksem.qamr._
import turksem.util._

import cats._
import cats.data._
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.PosTags
// import nlpdata.structure._
// import nlpdata.datasets.ptb._
// import nlpdata.datasets.wiki1k._
// import nlpdata.datasets.wiktionary.Inflections

// import akka.actor._
// import akka.stream.scaladsl.Flow
// import akka.stream.scaladsl.Source

// import scala.concurrent.duration._
// import scala.language.postfixOps

// import scala.util.Random

package object analysis {

  object AbstractSlot {
    implicit val abstractSlotShow: Show[AbstractSlot] = Show.show(_ => "_")
  }
  type AbstractSlot = AbstractSlot.type

  object Datasets {
    def readDataTSV(lines: Iterator[String]): QAData[SentenceId] = readTSV(lines, SentenceId.fromString)

    lazy val trainUnfiltered = readDataTSV(loadOutputFile("train.tsv").get.iterator)
    lazy val devUnfiltered = readDataTSV(loadOutputFile("dev.tsv").get.iterator)
    lazy val testUnfiltered = readDataTSV(loadOutputFile("test.tsv").get.iterator)
    lazy val ptbTrainUnfiltered = readDataTSV(loadOutputFile("ptb-train.tsv").get.iterator)
    lazy val ptbDevUnfiltered = readDataTSV(loadOutputFile("ptb-dev.tsv").get.iterator)
    lazy val ptbTestUnfiltered = readDataTSV(loadOutputFile("ptb-test.tsv").get.iterator)
    lazy val ptbAMRUnfiltered = readDataTSV(loadOutputFile("ptb-amr.tsv").get.iterator)

    def isQAGood(sqa: SourcedQA[SentenceId]): Boolean = sqa.isValid && beginsWithWh(sqa.question)

    lazy val train = trainUnfiltered.filterByQA(isQAGood)
    lazy val dev = devUnfiltered.filterByQA(isQAGood)
    lazy val test = testUnfiltered.filterByQA(isQAGood)
    lazy val ptbTrain = ptbTrainUnfiltered.filterByQA(isQAGood)
    lazy val ptbDev = ptbDevUnfiltered.filterByQA(isQAGood)
    lazy val ptbTest = ptbTestUnfiltered.filterByQA(isQAGood)
    lazy val ptbAMR = ptbAMRUnfiltered.filterByQA(isQAGood)
    lazy val ptbAll = new QAData(ptbTrain.all ++ ptbDev.all ++ ptbTest.all ++ ptbAMR.all)
  }

  import Datasets._
  import Reinflection._
  import TemplateToken._

  // QuestionTemplate is global across all sentences.

  case class ContiguousSpan(begin: Int, end: Int) {
    def contains(i: Int): Boolean = begin <= i || end >= i
    def getTokens(id: SentenceId): Vector[String] =
      id.tokens.slice(begin, end)
  }

  case class AlignmentState(
    mapping: Map[ContiguousSpan, ContiguousSpan],
    usedAlignments: Set[
      (QuestionTemplateAlignment[AbstractSlot],
       (QuestionTemplate[ContiguousSpan], ContiguousSpan))]
  ) {

    def referencesUsed = usedAlignments.map(_._1)
    def prevsUsed = usedAlignments.map(_._2)

    def get(cs: ContiguousSpan): Option[ContiguousSpan] =
      mapping.get(cs)

    def add(ref: ContiguousSpan, target: ContiguousSpan) =
      this.copy(mapping = this.mapping.updated(ref, target))

    def addAlignment(
      ref: QuestionTemplateAlignment[AbstractSlot],
      target: (QuestionTemplate[ContiguousSpan], ContiguousSpan)
    ) = this.copy(usedAlignments = this.usedAlignments + (ref -> target))
  }

  type AlignmentProcessor[A] = StateT[List, AlignmentState, A]
  def liftAlign[A](as: List[A]) = StateT.lift[List, AlignmentState, A](as)

  object AlignmentState {

    def empty: AlignmentState = AlignmentState(
      Map.empty[ContiguousSpan, ContiguousSpan],
      Set.empty[(QuestionTemplateAlignment[AbstractSlot],
                 (QuestionTemplate[ContiguousSpan], ContiguousSpan)
                )])

    // assumes templates match
    import StateT._
    def resolve(
      span: ContiguousSpan,
      reference: QuestionTemplateAlignment[AbstractSlot],
      target: (QuestionTemplate[ContiguousSpan], ContiguousSpan)
    ): StateT[List, AlignmentState, ContiguousSpan] = for {
      _ <- target._1.zipSlots(reference.alignments).traverse[AlignmentProcessor, ContiguousSpan] {
        case (targetSpan, refSpans) => resolveSpan(refSpans, targetSpan)
      }
      _ <- resolveSpan(reference.answers, target._2)
      curAlignment <- get[List, AlignmentState]
      result <- liftAlign(curAlignment.get(span).toList)
      _ <- modify[List, AlignmentState](_.addAlignment(reference, target))
    } yield result

    def resolveSpan(
      referenceSpans: List[ContiguousSpan],
      targetSpan: ContiguousSpan
    ): StateT[List, AlignmentState, ContiguousSpan] = for {
      curAlignment <- get[List, AlignmentState]
      refSpan <- liftAlign(referenceSpans)
      if curAlignment.get(refSpan).fold(false)(_ == targetSpan)
      _ <- set[List, AlignmentState](curAlignment.add(refSpan, targetSpan))
    } yield refSpan
  }

  def consistentSpanInstantiationsSimple(
    seedAlignment: QuestionTemplateAlignment[AbstractSlot],
    previousQAs: List[(QuestionTemplate[ContiguousSpan], ContiguousSpan)],
    referenceQTAs: List[QuestionTemplateAlignment[AbstractSlot]]
  ): List[(AlignmentState, QuestionTemplate[ContiguousSpan])] = {
    val filteredReferenceQTAs = referenceQTAs.filter(_ != seedAlignment)
    val alignmentProcessor = seedAlignment.template
      .replaceSlots(seedAlignment.alignments)
      .traverse[AlignmentProcessor, ContiguousSpan] { spans =>
      import StateT._
      for {
        span <- lift[List, AlignmentState, ContiguousSpan](spans)
        curAlignment <- get[List, AlignmentState]
        alignedSpan <- curAlignment.get(span) match {
          // if we already know the span from our mapping, use that
          case Some(result) => pure[List, AlignmentState, ContiguousSpan](result)
          case None => for {
            // otherwise, identify an as-of-yet unused reference that uses this span,
            ref <- liftAlign(filteredReferenceQTAs)
            if !curAlignment.referencesUsed.contains(ref) && ref.alignments.exists(_.contains(span))
            // an as-of-yet unused target previous QA that can be aligned with that reference,
            target <- liftAlign(previousQAs)
            if !curAlignment.prevsUsed.contains(target) && target.as(AbstractSlot) == ref.template
            // and a valid alignment between the reference and target that resolves the desired span.
            result <- AlignmentState.resolve(span, ref, target)
          } yield result
        }
      } yield alignedSpan
    }
    alignmentProcessor.run(AlignmentState.empty)
  }

  def readFirstSpanDirectly[Slot](id: SentenceId)(slot: Slot, spans: List[ContiguousSpan]) =
    spans.head.getTokens(id).map(_.lowerCase)

  def inflectFirstSpan(id: SentenceId)(slot: Reinflection, spans: List[ContiguousSpan]) = {
    val posTaggedSentence = posTag(id.tokens)
    val posTaggedSpan = posTaggedSentence
      .slice(spans.head.begin, spans.head.end)
      .toList
    val lowerCaseSpan = posTaggedSpan.map(_.token.lowerCase)

    def getGoodInflectedIndices(admissiblePosTags: Set[String]) = posTaggedSpan.indicesYielding {
      case POSTaggedToken(t, pos) =>
        val isGood = admissiblePosTags.contains(pos)
        inflections.getInflectedForms(t.lowerCase).filter(const(isGood)).map(_.allForms)
    }.toList

    def inflectFirstOccurrence(form: Int, pairs: List[(Int, List[LowerCaseString])]) = {
      pairs.filter {
        case (index, inflections) => inflections(form) != lowerCaseSpan(index)
      }.headOption.fold(lowerCaseSpan) {
        case (index, inflections) => lowerCaseSpan.updated(index, inflections(form))
      }.toVector
    }

    slot match {
      case NoReinflection => lowerCaseSpan
      case VerbReinflection(form) => inflectFirstOccurrence(form, getGoodInflectedIndices(ptbVerbPosTags))
      case NounReinflection(form) => inflectFirstOccurrence(form, getGoodInflectedIndices(ptbNounPosTags))
    }
  }

  def checkTemplateIntegrity[Slot : Show](
    templateAlignments: Iterator[QuestionTemplateAlignment[Slot]],
    instantiateSlot: (SentenceId => (Slot, List[ContiguousSpan]) => Vector[LowerCaseString]) =
      (id: SentenceId) => (slot: Slot, spans: List[ContiguousSpan]) => readFirstSpanDirectly[Slot](id)(slot, spans),
    verbose: Boolean = false) = {
    var incorrectInstantiations = 0
    var totalInstantiations = 0
    templateAlignments.foreach { case QuestionTemplateAlignment(sqa, template, alignments) =>
      val instance = template.zipSlots(alignments)
        .map(Function.tupled(instantiateSlot(sqa.id.sentenceId)))
        .getTokensLower
      if(instance != tokenize(sqa.question).map(_.lowerCase)) {
        incorrectInstantiations = incorrectInstantiations + 1
        if(verbose) {
          System.err.println("Template instance does not match question tokens:")
          System.err.println(s"\tSentence:  ${Text.render(sqa.id.sentenceId.tokens)}")
          System.err.println(s"\tReference: ${Text.render(tokenize(sqa.question).map(_.toLowerCase))}")  // wouldn't need map if had liskov
          System.err.println(s"\tTemplate:  ${template.show}")
          System.err.println(s"\tInstance:  ${Text.render(instance.map(_.toString))}") // wouldn't need map if had liskov
        }
      }
      totalInstantiations = totalInstantiations + 1
    }

    println(s"Total templates: $totalInstantiations")
    println(s"Incorrectly instantiated: $incorrectInstantiations (${incorrectInstantiations * 100.0 / totalInstantiations}%)")
  }

  def histogramString(hist: Scorer[Int, Int]): String = {
    val vec = (0 to hist.keyIterator.max).map(hist.get).toVector
    val max = vec.max
    val scaleMax = 50.0
    val scaleFactor = scaleMax / max
    def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
    def pounds(n: Int) = "#" * n
    vec.zipWithIndex
      .map { case (n, i) => f"$i%3d|${pounds(scale(n))}%s $n%d"}
      .mkString("\n")
  }

  def analyzeTemplates(templateAnalysis: TemplateAnalysis) = {
    val templates = templateAnalysis.alignmentsByTemplate

    val totalQAs = templates.iterator.map(_._2.size).sum
    println(s"Total QAs: $totalQAs")

    val totalTemplates = templates.size
    println(s"Total templates: $totalTemplates")

    val templateSizes = Scorer[Int, Int](
      templates.iterator.map(_._1.size.toInt)
    )
    println(s"Template sizes:\n${histogramString(templateSizes)}")

    val templateInstanceSizes = Scorer[Int, Int](
      templates.iterator.flatMap {
        case (template, sqas) => sqas.iterator.map(_ => template.size.toInt)
      }
    )
    println(s"Template instance sizes:\n${histogramString(templateInstanceSizes)}")

    val templatesByFrequencyDecreasing = templates.toVector.sortBy(-_._2.size)
    println("Most common templates:")
    templatesByFrequencyDecreasing.zipWithIndex.take(100).foreach { case ((template, alignments), index) =>
      val questionsWithSentences = alignments.take(3).map(alignment =>
        f"${alignment.question}%-50s  ${Text.render(alignment.sourcedQA.id.sentenceId)}%s"
      ).mkString("\n")
      println(f"\n$index%2d: ${template.show}%s \t ${alignments.size}%d \n$questionsWithSentences%s")
    }

    println
    println("Least common templates:")
    templatesByFrequencyDecreasing.takeRight(25).foreach { case (template, alignments) =>
      println(s"${template.show} \t ${alignments.size} \t ${alignments.head.question} \t ${Text.render(alignments.head.sourcedQA.id.sentenceId.tokens)}")
    }

    val cumulativeCoverage = templatesByFrequencyDecreasing.scanLeft(0)(_ + _._2.size)

    def coverageOf(percent: Double) = cumulativeCoverage.takeWhile(_ < (totalQAs * percent)).size
    println(s"10% coverage with ${coverageOf(0.10)} templates")
    println(s"20% coverage with ${coverageOf(0.20)} templates")
    println(s"25% coverage with ${coverageOf(0.25)} templates")
    println(s"30% coverage with ${coverageOf(0.3)} templates")
    println(s"40% coverage with ${coverageOf(0.4)} templates")
    println(s"50% coverage with ${coverageOf(0.5)} templates")
    println(s"60% coverage with ${coverageOf(0.6)} templates")
    println(s"70% coverage with ${coverageOf(0.7)} templates")
    println(s"80% coverage with ${coverageOf(0.8)} templates")
    println(s"90% coverage with ${coverageOf(0.9)} templates")
    println(s"95% coverage with ${coverageOf(0.95)} templates")
    println(s"100% coverage with ${coverageOf(1.0)} templates")

    def occurringNTimes(n: Int) = templatesByFrequencyDecreasing.iterator.filter(_._2.size == n)
    println(s"Number of templates occuring once: ${occurringNTimes(1).size}");
    (2 to 10).foreach { i =>
      println(s"Number of templates occuring $i times: ${occurringNTimes(i).size}")
    }

    println("Example templates occurring twice:")
    occurringNTimes(2).take(25).foreach { case (template, alignments) =>
      println(s"${template.show} \t ${alignments.size} \t ${alignments.head.question} \t ${Text.render(alignments.head.sourcedQA.id.sentenceId.tokens)}")
    }

    // analyzing template vocabulary

    def getWordsInTemplate[S](t: QuestionTemplate[S]) = t.templateTokens.collect {
      case TemplateString(s) => s
    }

    val templateVocabCounts = Scorer[LowerCaseString, Int](templatesByFrequencyDecreasing.iterator.flatMap(p => getWordsInTemplate(p._1).iterator))
    val templateVocabCountsDecreasing = templateVocabCounts.iterator.toVector.sortBy(-_._2)
    println(s"Template vocabulary includes ${templateVocabCounts.sum} occurrences of ${templateVocabCounts.size} words")
    // println("Most common template vocabulary: ")
    // templateVocabCountsDecreasing.iterator.take(100).foreach { case (word, count) =>
    //   println(s"$word \t $count")
    // }

    val templateWeightedVocabCounts = Scorer[LowerCaseString, Int]
    templatesByFrequencyDecreasing.foreach { case (t, occurrences) =>  getWordsInTemplate(t).foreach(w => templateWeightedVocabCounts.add(w, occurrences.size)) }
    val templateWeightedVocabCountsDecreasing = templateWeightedVocabCounts.iterator.toVector.sortBy(-_._2)
    // println("Most common template vocabulary (occurrences): ")
    // templateWeightedVocabCountsDecreasing.iterator.take(100).foreach { case (word, count) =>
    //   println(s"$word \t $count")
    // }

    val nonStopwordTemplateVocabCountsDecreasing = templateWeightedVocabCountsDecreasing.filter(p => !isStopword(p._1))
    // println("Most common non-stopword template vocabulary (occurrences): ")
    // nonStopwordTemplateVocabCountsDecreasing.iterator.take(100).foreach { case (word, count) =>
    //   println(s"$word \t $count")
    // }

    // subanalysis of common templates

    val commonTemplates = templateAnalysis.mostCommonTemplates.toVector
    val commonTemplateVocabCounts = Scorer[LowerCaseString, Int](commonTemplates.iterator.flatMap(getWordsInTemplate))
    val commonTemplateVocabCountsDecreasing = commonTemplateVocabCounts.iterator.toVector.sortBy(-_._2)
    println(s"Common template vocabulary includes ${commonTemplateVocabCounts.sum} occurrences of ${commonTemplateVocabCounts.size} words")
    // println("[Most common] [common template] vocabulary: ")
    // templateVocabCountsDecreasing.iterator.take(100).foreach { case (word, count) =>
    //   println(s"$word \t $count")
    // }

    val mostCommonTemplates = commonTemplates.toSet
    val proportionTemplatesCoveredByCommon = mostCommonTemplates.size.toDouble / totalTemplates
    val proportionInstancesCoveredByCommon = templates.collect {
      case (template, qtas) if mostCommonTemplates.contains(template) => qtas.size
    }.sum
    println(f"Proportion of templates covered: ${proportionTemplatesCoveredByCommon * 100}%.2f")
    println(f"Proportion of template instances covered: ${proportionInstancesCoveredByCommon * 100}%.2f")

  }

  def sentenceSpecificSearchAnalysis[Slot : Show](
    sentenceId: SentenceId,
    templatesForSentence: List[QuestionTemplateAlignment[Slot]],
    verbose: Boolean = true
  ) = {
    val sentenceTokens = sentenceId.tokens
    val posTaggedSentenceTokens = posTag(sentenceTokens)

    var qasSoFar = List.empty[QuestionTemplateAlignment[Slot]]
    var spansCovered = Set.empty[ContiguousSpan]
    var frontier = templatesForSentence.filter(qta => qta.template.size == 1)
    var remaining = templatesForSentence.toSet
    var done = false

    if(verbose) {
      System.out.println(Text.render(sentenceTokens))
      System.out.println(s"Total number of questions: ${templatesForSentence.size}")
    }

    while(frontier.nonEmpty) {
      if(verbose) {
        System.out.println(s"So far: ${qasSoFar.size}")
        System.out.println(s"Frontier: ${frontier.size}:")
        frontier.foreach(qta =>
          System.out.println(s"\t${qta.template.show}\t${qta.question}")
        )
      }

      qasSoFar ++= frontier
      spansCovered ++= frontier.flatMap(qta =>
        qta.alignments.flatten ++ qta.answers
      )
      remaining --= frontier
      frontier = remaining.filter(qta =>
        qta.alignments.forall(_.exists(spansCovered.contains))
      ).toList

      // if(verbose) {
      //   System.out.println
      //   System.out.println("Covered spans:")
      //   spansCovered.foreach { case ContiguousSpan(begin, end) =>
      //     println(s"$begin:$end\t" + Text.renderSpan(sentenceId.tokens, (begin to end).toSet))
      //   }
      // }
    }

    System.out.println(s"Remaining: ${remaining.size}")
    remaining.foreach(qta =>
      System.out.println(s"\t${qta.template.show}\t${qta.question}")
    )
  }

  // simple af search process; only do exact span matching
  // also only deal with simplest templates for now
  def searchSentenceWithReferenceSimple(
    allowedTemplates: Set[QuestionTemplate[AbstractSlot]],
    reference: Map[SentenceId, List[QuestionTemplateAlignment[AbstractSlot]]],
    sentenceId: SentenceId,
    templatesForSentence: List[QuestionTemplateAlignment[AbstractSlot]],
    verbose: Boolean = true) = {

    val (commonTemplatesForSentence, givenUpTemplatesForSentence) = templatesForSentence
      .partition(qta => allowedTemplates.contains(qta.template))

    var qasSoFar = List.empty[QuestionTemplateAlignment[AbstractSlot]]
    var frontier = Scorer[QuestionTemplate[ContiguousSpan], Int](
      commonTemplatesForSentence
        .filter(_.template.size <= 1)
        .map(qta => qta.template.replaceSlots(qta.alignments).map(_.head))
        .iterator)

    var remaining = commonTemplatesForSentence.toSet
    var thrownAway = Set.empty[QuestionTemplate[ContiguousSpan]]

    if(verbose) {
      val sentenceTokens = sentenceId.tokens
      System.out.println(Text.render(sentenceTokens))
      System.out.println(s"Total number of questions: ${templatesForSentence.size} (${commonTemplatesForSentence.size} common)")
      System.out.println(s"Given up on ${givenUpTemplatesForSentence.size} questions:")
      givenUpTemplatesForSentence.foreach { qta =>
        val question = qta.template
          .replaceSlots(qta.alignments)
          .map(_.head)
          .fillSpansId(sentenceId)
          .mkString(" ")
        val answer = qta.answers.head.getTokens(sentenceId).mkString(" ")
        System.out.println(s"\t${qta.template.show}\t${question}")
      }
    }

    while(frontier.nonEmpty) {

      val sortedScoredFrontier = frontier.iterator.toVector.sortBy(-_._2)

      if(verbose) {
        System.out.println(s"So far: ${qasSoFar.size}")
      }

      val goodQuestions = {
        for {
          qta <- remaining
          (iq, count) <- sortedScoredFrontier.find(pair => qta.matches(pair._1)).toList
        } yield (qta, iq, count)
      }.toList

      if(verbose) {
        System.out.println(s"${goodQuestions.map(_._2).size} good ones:")
        goodQuestions.foreach { case (qta, template, count) =>
          val question = template.fillSpansId(sentenceId).mkString(" ")
          val answer = qta.answers.head.getTokens(sentenceId).mkString(" ")
          System.out.println(s"${count}\t${template.as(AbstractSlot).show}\t${question}\t${answer}")
        }
      }

      qasSoFar ++= goodQuestions.map(_._1)
      remaining --= goodQuestions.map(_._1)

      val missedQuestions = sortedScoredFrontier.filter {
        case (t1, count) => !goodQuestions.exists {
          case (_, t2, _) => t1 == t2
        }
      }
      thrownAway ++= missedQuestions.map(_._1)
      if(verbose) {
        System.out.println(s"Unused questions on frontier:")
        missedQuestions.foreach { case (spanTemplate, count) =>
          System.out.println(
            count +
              "\t" + spanTemplate.as(AbstractSlot).show +
              "\t" + spanTemplate.fillSpansId(sentenceId).mkString(" ")
          )
        }
      }

      val coveredTemplates = qasSoFar.map(_.template).toSet
      frontier = Scorer[QuestionTemplate[ContiguousSpan], Int](
        reference.iterator.flatMap { case (id, qtas) =>
          val alignableQTAs = qtas.filter(qta => coveredTemplates.contains(qta.template))
          val coveredReferenceSpans = alignableQTAs.flatMap(qta => qta.alignments.flatten ++ qta.answers).toSet
          val potentialQTAs = qtas.filter(qta =>
            qta.template.size > 1 &&
              qta.alignments.forall(choices => choices.exists(coveredReferenceSpans.contains))
          )
          val alignmentResults = potentialQTAs.flatMap { qta =>
            consistentSpanInstantiationsSimple(
              qta,
              goodQuestions.map { case (qta, iq, count) => (iq, qta.answers.head) }.toSet.toList,
              alignableQTAs)
          }
          if(verbose) {
            // print stuff about alignment results
          }
          alignmentResults.map(_._2).toSet.iterator // count templates by num sentences inducing them
        }.filterNot(thrownAway.contains)
      )
    }

    System.out.println(s"Remaining: ${remaining.size}")
    remaining.foreach(qta =>
      System.out.println(s"\t${qta.template.show}\t${qta.question}")
    )
  }

  def templatizeQuestionContiguousSlotsWithReinflectionAndAlignment(
    sqa: SourcedQA[SentenceId]
  ): QuestionTemplateAlignment[Reinflection] = {

    case class AlignedReinflection(
      index: Int,
      reinflection: Reinflection)

    val sentenceTokens = sqa.id.sentenceId.tokens
    val posTaggedSentenceTokens = posTag(sentenceTokens)
    val qTokens = {
      val toks = tokenize(sqa.question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val qAlignments: Map[Int, List[AlignedReinflection]] =
      getReinflectedQuestionSentenceAlignments(sentenceTokens, qTokens).map {
        case (qi, inflectedAlignments) => qi -> inflectedAlignments.map {
          case InflectedAlignment(si, reinflectionOpt) =>
            if(ptbNounPosTags.contains(posTaggedSentenceTokens(si).pos)) {
              AlignedReinflection(si, reinflectionOpt.fold(noReinflection)(nounReinflection(_)))
            } else if(ptbVerbPosTags.contains(posTaggedSentenceTokens(si).pos)) {
              AlignedReinflection(si, reinflectionOpt.fold(noReinflection)(verbReinflection(_)))
            } else {
              AlignedReinflection(si, noReinflection) // don't bother trying to reinflect non-verb/nouns
            }
        }
      }.withDefaultValue(Nil)

    case class TemplatingState(
      resolvedTail: List[TemplateToken[Reinflection]],
      resolvedAlignments: List[List[ContiguousSpan]],
      unresolvedTokens: List[String],
      unresolvedAlignedIndices: Set[AlignedReinflection])
    object TemplatingState {
      def empty = TemplatingState(Nil, Nil, Nil, Set.empty[AlignedReinflection])
    }

    val templateState = qTokens.zipWithIndex.foldRight(TemplatingState.empty) { case ((token, index), state) =>
      val sIndices = if(isStopword(token)) {
        sentenceTokens.zipWithIndex.collect {
          case (sToken, sIndex) if token.equalsIgnoreCase(sToken) =>
            AlignedReinflection(sIndex, noReinflection)
        }.toSet
      } else qAlignments(index).toSet

      val newAlignments: Set[AlignedReinflection] = state.unresolvedAlignedIndices.flatMap {
        case AlignedReinflection(curAlignedIndex, reinflection) =>
          val newAlignedIndex = curAlignedIndex - 1
          sIndices.find(_.index == newAlignedIndex).map(_.reinflection).map {
            case NoReinflection => AlignedReinflection(newAlignedIndex, reinflection)
            case newReinflection =>
              // if(reinflectionOpt.nonEmpty) {
              //   System.err.println("Two reinflections in single span! defaulting to first")
              // }
              AlignedReinflection(newAlignedIndex, newReinflection)
          }
      }

      if(state.unresolvedTokens.isEmpty) {
        if(sIndices.nonEmpty) {
          TemplatingState(state.resolvedTail, state.resolvedAlignments, token :: state.unresolvedTokens, sIndices)
        } else {
          TemplatingState(TemplateString(token.lowerCase) :: state.resolvedTail, state.resolvedAlignments, state.unresolvedTokens, Set.empty[AlignedReinflection])
        }
      } else if(newAlignments.nonEmpty) {
        TemplatingState(state.resolvedTail, state.resolvedAlignments, token :: state.unresolvedTokens, newAlignments)
      } else {
        val (resolvedTail, resolvedAlignedSpans) = if(state.unresolvedTokens.forall(isStopword)) {
          (state.unresolvedTokens.map(t => TemplateString(t.lowerCase)) ++ state.resolvedTail,
           state.resolvedAlignments)
        } else {
          val reinflection = if(state.unresolvedAlignedIndices.isEmpty) {
            System.err.println("Warning: unresolved aligned indices should not be empty")
            noReinflection
          } else if(state.unresolvedAlignedIndices.exists(_.reinflection == noReinflection)) {
            noReinflection
          } else {
            state.unresolvedAlignedIndices.map(_.reinflection).headOption.getOrElse(noReinflection)
          }
          val alignedSpans = state.unresolvedAlignedIndices
            .filter(_.reinflection == reinflection).toList
            .map(_.index)
            .map(i => ContiguousSpan(i, i + state.unresolvedTokens.size))
          (TemplateSlot(reinflection) :: state.resolvedTail,
           alignedSpans :: state.resolvedAlignments)
        }
        if(sIndices.isEmpty) {
          TemplatingState(TemplateString(token.lowerCase) :: resolvedTail, resolvedAlignedSpans, Nil, Set.empty[AlignedReinflection])
        } else {
          TemplatingState(resolvedTail, resolvedAlignedSpans, List(token), sIndices)
        }
      }
    }

    val (templateTokens, alignments) = if(templateState.unresolvedTokens.nonEmpty) {
      val reinflection = if(templateState.unresolvedAlignedIndices.isEmpty) {
        System.err.println("Warning: unresolved aligned indices should not be empty")
        noReinflection
      } else if(templateState.unresolvedAlignedIndices.exists(_.reinflection == noReinflection)) {
        noReinflection
      } else {
        templateState.unresolvedAlignedIndices.map(_.reinflection).headOption.getOrElse(noReinflection)
      }
      if(templateState.unresolvedTokens.forall(isStopword)) {
        (templateState.unresolvedTokens.map(t => TemplateString(t.lowerCase)) ++ templateState.resolvedTail,
         templateState.resolvedAlignments)
      } else {
        val alignedSpans = templateState.unresolvedAlignedIndices
          .filter(_.reinflection == reinflection).toList
          .map(_.index)
          .map(i => ContiguousSpan(i, i + templateState.unresolvedTokens.size))
        (TemplateSlot(reinflection) :: templateState.resolvedTail,
         alignedSpans :: templateState.resolvedAlignments)
      }
    } else (templateState.resolvedTail, templateState.resolvedAlignments)

    QuestionTemplateAlignment(sqa, QuestionTemplate(templateTokens), alignments)
  }

  def templatizeQuestionContiguousSlotsWithAlignment(
    sqa: SourcedQA[SentenceId]
  ): QuestionTemplateAlignment[AbstractSlot] = {
    val alignment = templatizeQuestionContiguousSlotsWithReinflectionAndAlignment(sqa)
    alignment.copy(template = alignment.template.as(AbstractSlot))
  }

  def naiveTemplatizeQuestion(id: SentenceId, question: String): QuestionTemplate[AbstractSlot] = {
    val sentenceTokens = id.tokens
    val qTokens = {
      val toks = tokenize(question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val alignedQTokens = getAlignedQuestionIndices(sentenceTokens, qTokens)
    val templateTokens = qTokens.zipWithIndex.foldRight(List.empty[TemplateToken[AbstractSlot]]) {
      case ((token, index), templateTail) =>
        if(alignedQTokens.contains(index)) templateTail match {
          case TemplateSlot(AbstractSlot) :: _ => templateTail // collapses into template slot
          case _ => TemplateSlot(AbstractSlot) :: templateTail
        } else TemplateString(token.lowerCase) :: templateTail
    }
    QuestionTemplate(templateTokens)
  }

  // templatization with smarter treatment of stopwords

  def templatizeQuestionContiguousSlots(
    id: SentenceId, question: String
  ): QuestionTemplate[AbstractSlot] = {
    val sentenceTokens = id.tokens
    val qTokens = {
      val toks = tokenize(question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val qAlignments: Map[Int, List[Int]] = getQuestionSentenceAlignments(sentenceTokens, qTokens).toList.groupBy(_._1).map {
      case (qIndex, pairs) => qIndex -> pairs.map(_._2)
    }.withDefaultValue(Nil)

    case class TemplatingState(
      resolvedTail: List[TemplateToken[AbstractSlot]],
      unresolvedTokens: List[String],
      unresolvedAlignedIndices: Set[Int])
    object TemplatingState {
      def empty = TemplatingState(Nil, Nil, Set.empty[Int])
    }

    val templateState = qTokens.zipWithIndex.foldRight(TemplatingState.empty) { case ((token, index), state) =>
      val sIndices = if(isStopword(token)) {
        sentenceTokens.zipWithIndex.collect {
          case (sToken, sIndex) if token.equalsIgnoreCase(sToken) => sIndex
        }.toSet
      } else qAlignments(index).toSet

      val newAlignments = state.unresolvedAlignedIndices.map(_ - 1) intersect sIndices

      if(state.unresolvedTokens.isEmpty) {
        if(sIndices.nonEmpty) {
          TemplatingState(state.resolvedTail, token :: state.unresolvedTokens, sIndices)
        } else {
          TemplatingState(TemplateString(token.lowerCase) :: state.resolvedTail, state.unresolvedTokens, Set.empty[Int])
        }
      } else if(newAlignments.nonEmpty) {
        TemplatingState(state.resolvedTail, token :: state.unresolvedTokens, newAlignments)
      } else {
        val resolvedTail = if(state.unresolvedTokens.forall(isStopword)) {
          state.unresolvedTokens.map(t => TemplateString(t.lowerCase)) ++ state.resolvedTail
        } else {
          TemplateSlot(AbstractSlot) :: state.resolvedTail
        }
        if(sIndices.isEmpty) {
          TemplatingState(TemplateString(token.lowerCase) :: resolvedTail, Nil, Set.empty[Int])
        } else {
          TemplatingState(resolvedTail, List(token), sIndices)
        }
      }
    }
    val templateTokens = if(templateState.unresolvedTokens.nonEmpty) {
      if(templateState.unresolvedTokens.forall(isStopword)) templateState.unresolvedTokens.map(t => TemplateString(t.lowerCase)) ++ templateState.resolvedTail
      else TemplateSlot(AbstractSlot) :: templateState.resolvedTail
    } else templateState.resolvedTail
    QuestionTemplate(templateTokens)
  }

  def templatizeQuestionSingleWord(
    sqa: SourcedQA[SentenceId]
  ): Option[QuestionTemplateAlignment[AbstractSlot]] = {
    val sentenceTokens = sqa.id.sentenceId.tokens
    val posTaggedSentenceTokens = posTag(sentenceTokens)
    val qTokens = {
      val toks = tokenize(sqa.question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val alignments = getQuestionSentenceAlignments(sentenceTokens, qTokens)
    val alignedQIndices = alignments.map(_._1).toSet.size
    if(alignedQIndices != 1) None else Some {
      val alignedQIndex = alignments.head._1
      val alignedSentenceIndices = alignments.map(_._2).toList.sorted
      val alignedSentenceSpans = alignedSentenceIndices.map(i => ContiguousSpan(i, i))
      val templateTokens = qTokens.toList
        .map(tok => TemplateString(tok.lowerCase))
        .updated(alignedQIndex, TemplateSlot(AbstractSlot))
      QuestionTemplateAlignment(sqa, QuestionTemplate(templateTokens), List(alignedSentenceSpans))
    }
  }

  // better for when splitting templates by pos tag
  def templatizeQuestionSingleWordWithPOS(
    sqa: SourcedQA[SentenceId]
  ): Option[QuestionTemplateAlignment[String]] = {
    val sentenceTokens = sqa.id.sentenceId.tokens
    val posTaggedSentenceTokens = posTag(sentenceTokens)
    val qTokens = {
      val toks = tokenize(sqa.question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val alignments = getReinflectedQuestionSentenceAlignments(sentenceTokens, qTokens)
    val alignedQIndices = alignments.keySet.size
    if(alignedQIndices != 1) None else Some {
      val (alignedQIndex, possiblyReinflectedSentenceAlignments) = alignments.head
      val alignedSentenceIndicesWithSameToken = possiblyReinflectedSentenceAlignments.collect {
        case InflectedAlignment(i, None) => i
      }.sorted
      val (pos, finalAlignments) = alignedSentenceIndicesWithSameToken.headOption match { // choose first arbitrarily
        case Some(sIndex) => (posTaggedSentenceTokens(sIndex).pos, alignedSentenceIndicesWithSameToken)
        case None => // must have changed the form of the word
          val reinflectedSentenceAlignments = possiblyReinflectedSentenceAlignments.collect {
            case InflectedAlignment(i, Some(form)) => (i, form)
          }
          val (chosenSIndex, chosenReinflection) = reinflectedSentenceAlignments.head
          val alignedPOS = posTaggedSentenceTokens(chosenSIndex).pos // choose first arbitrarily. this always exists
          val chosenPOS = if(PosTags.verbPosTags.contains(alignedPOS)) chosenReinflection match {
            case 0 => "VB" // might also be VBP, but whatever. rarely matters
            case 1 => "VBZ"
            case 2 => "VBG"
            case 3 => "VBD"
            case 4 => "VBN"
            // should never happen, but whatever:
            case _ => alignedPOS
          } else if(PosTags.nounPosTags.contains(alignedPOS)) chosenReinflection match {
            // handle common cases of pluralizing nouns
            case 0 => if(alignedPOS.startsWith("NNP")) "NNP" else "NN"
            case 1 => if(alignedPOS.startsWith("NNP")) "NNPS" else "NNS"
            // ... but in these cases we're typically turning them into verbs
            case 2 => "VBG"
            case 3 => "VBD"
            case 4 => "VBN"
            // should never happen, but whatever:
            case _ => alignedPOS
          } else alignedPOS
          (chosenPOS, reinflectedSentenceAlignments.map(_._1))
      }
      val alignedSentenceSpans = finalAlignments.map(i => ContiguousSpan(i, i))
      val templateTokens = qTokens.toList
        .map(tok => TemplateString(tok.lowerCase))
        .updated(alignedQIndex, TemplateSlot(pos))
      QuestionTemplateAlignment(sqa, QuestionTemplate(templateTokens), List(alignedSentenceSpans))
    }
  }


  class TemplateAnalysis(data: QAData[SentenceId]) {

    lazy val naiveTemplateMap = data.all.groupBy(sqa =>
      naiveTemplatizeQuestion(sqa.id.sentenceId, sqa.question)
    )

    lazy val contigTemplateMap = data.all.groupBy(sqa =>
      templatizeQuestionContiguousSlots(sqa.id.sentenceId, sqa.question)
    )

    lazy val contigReinflectedTemplateMapWithAlignment = data.all.groupBy(sqa =>
      templatizeQuestionContiguousSlotsWithReinflectionAndAlignment(sqa).template
    )

    lazy val contigTemplateMapWithAlignment = data.all.groupBy(sqa =>
      templatizeQuestionContiguousSlotsWithAlignment(sqa).template
    )

    lazy val singleWordTemplateAlignments = data.all.flatMap(templatizeQuestionSingleWord)
    lazy val singleWordAlignmentsByTemplate = singleWordTemplateAlignments.groupBy(_.template)

    // label e.g. train
    def writeSingleWordTemplateTSV(label: String) = {
      val templatesByFrequencyDecreasing = singleWordAlignmentsByTemplate.toVector.sortBy(-_._2.size)
      val sb = new StringBuilder
      templatesByFrequencyDecreasing.foreach {
        case (template, qtas) =>
          sb.append(s"${template.show}\t${qtas.size}\n")
          qtas.take(10).foreach { qta =>
            val id = qta.sourcedQA.id.sentenceId
            sb.append(s"\t${qta.sourcedQA.question}\t${Text.renderSpan(id, qta.sourcedQA.wqa.answer)}\t${Text.render(id)}\n")
          }
          sb.append("\n")
      }
      saveOutputFile(s"$label-1word-templates.tsv", sb.toString)
    }

    lazy val singleWordPOSTemplateAlignments = data.all.flatMap(templatizeQuestionSingleWordWithPOS)
    lazy val singleWordPOSAlignmentsByTemplate = singleWordPOSTemplateAlignments.groupBy(_.template)

    def writeSingleWordWithPOSTemplateTSVs(label: String) = {
      val templatesByFrequencyDecreasing = singleWordPOSAlignmentsByTemplate.toVector.sortBy(-_._2.size)
      var sb = new StringBuilder
      templatesByFrequencyDecreasing.foreach {
        case (template, qtas) =>
          sb.append(s"${template.show}\t${qtas.size}\n")
          qtas.take(10).foreach { qta =>
            val id = qta.sourcedQA.id.sentenceId
            sb.append(s"\t${qta.sourcedQA.question}\t${Text.renderSpan(id, qta.sourcedQA.wqa.answer)}\t${Text.render(id)}\n")
          }
          sb.append("\n")
      }
      saveOutputFile(s"$label-pos-templates-freq.tsv", sb.toString)

      val templatesByType = singleWordPOSAlignmentsByTemplate.groupBy(_._1.foldMap(Option(_)))
      sb = new StringBuilder
      templatesByType.foreach {
        case (posOpt, templatesWithQTAs) =>
          sb.append(posOpt.get + "\n")
          templatesWithQTAs.toVector.sortBy(-_._2.size).foreach {
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
      saveOutputFile(s"$label-pos-templates-type.tsv", sb.toString)
    }

    // Current choice: single word, no reinflection
    lazy val allTemplateAlignments = data.all.flatMap(templatizeQuestionSingleWordWithPOS)
    lazy val alignmentsBySentence = allTemplateAlignments.groupBy(_.sentenceId)
    lazy val alignmentsByTemplate = allTemplateAlignments.groupBy(_.template)
    lazy val mostCommonTemplates = alignmentsByTemplate.collect {
      case (template, alignments) if alignments.size > 5 => template
    }.toSet

    // def searchSentenceSimple(
    //   sentenceId: SentenceId,
    //   templatesForSentence: List[QuestionTemplateAlignment[AbstractSlot]],
    //   verbose: Boolean = true) =
    //   searchSentenceWithReferenceSimple(
    //     mostCommonTemplates,
    //     alignmentsBySentence,
    //     sentenceId,
    //     templatesForSentence,
    //     verbose)

    // def analyzeAlignedTemplates(alignedTemplatesBySentence: Map[SentenceId, List[QuestionTemplateAlignment]]) = {
    //   val results = alignedTemplatesBySentence

    // }

    // Further abstractions: not necessary for now

    // val christenedWords = Set("type", "kind", "much", "many", "there").map(_.lowerCase)

    // def abstractExternalPhrases(templatize: (SentenceId, String) => QuestionTemplate)(id: SentenceId, question: String): QuestionTemplate = {
    //   templatize(id, question).foldRight(List.empty[TemplateToken]) {
    //     case (TemplateString(s), template) if isStopword(s) || whWords.contains(s) || christenedWords.contains(s) => TemplateString(s) :: template
    //     case (TemplateString(s), TemplateExt :: templateTail) => TemplateExt :: templateTail
    //     case (TemplateString(s), template) => TemplateExt :: template
    //     case (x, template) => x :: template
    //   }
    // }

    // def abstractPronouns(templatize: (SentenceId, String) => QuestionTemplate)(id: SentenceId, question: String): QuestionTemplate = {
    //   templatize(id, question).map {
    //     case TemplateString(s) if pronouns.contains(s) => TemplatePro
    //     case x => x
    //   }
    // }

    // lazy val contigExtTemplateMap = data.all.groupBy(sqa =>
    //   abstractExternalPhrases(templatizeQuestionContiguousSlots)(sqa.id.sentenceId, sqa.question)
    // )

    // lazy val contigProTemplateMap = data.all.groupBy(sqa =>
    //   abstractPronouns(templatizeQuestionContiguousSlots)(sqa.id.sentenceId, sqa.question)
    // )
  }

  // expect that Show is injective
  def makeTemplateTSV[Slot : Show](
    alignmentsBySentence: Map[SentenceId, List[QuestionTemplateAlignment[Slot]]],
    allowedTemplates: Set[QuestionTemplate[Slot]]
  ): String = {
    val sb = new java.lang.StringBuilder
    alignmentsBySentence.foreach { case (id, alignments) =>
      alignments.foreach { qta =>
        if(allowedTemplates.contains(qta.template)) {
          sb.append(SentenceId.toString(id) + "\t" + qta.template.show + "\n")
        }
      }
    }
    sb.toString
  }

  // def instantiateTemplate(
  //   posTaggedSentence: Vector[POSTaggedToken], template: QuestionTemplate, alignments: List[ContiguousSpan]
  // ): Vector[LowerCaseString] = template.foldRight((List.empty[LowerCaseString], alignments.reverse)) {
  //   case (TemplateSlot(reinflectionOpt), (tokensSoFar, nextAlignment :: remainingAlignments)) =>
  //     val span = posTaggedSentence
  //       .map(_.token.lowerCase)
  //       .zipWithIndex
  //       .slice(nextAlignment.begin, nextAlignment.end)
  //       .toList
  //     val inflectedSpan = reinflectionOpt match {
  //       case None => span.map(_._1)
  //       case Some(VerbReinflection(form)) => span.indicesYielding { case (w, index) =>
  //         val isVerb = ptbVerbPosTags.contains(posTaggedSentence(index).pos)
  //         inflections.getInflectedForms(w).filter(const(isVerb))
  //       }.filter { case (index, inflections) => inflections(form) != span(index)._1 } // and require that we change the form
  //           .headOption
  //           .fold(span.map(_._1)) { case (index, inflections) => span.map(_._1).updated(index, inflections(form)) }
  //       case Some(NounReinflection(form)) => span.indicesYielding { case (w, index) =>
  //         val isNoun = ptbNounPosTags.contains(posTaggedSentence(index).pos)
  //         inflections.getInflectedForms(w).filter(const(isNoun))
  //       }.filter { case (index, inflections) => inflections(form) != span(index)._1 } // and require that we change the form
  //           .headOption
  //           .fold(span.map(_._1)) { case (index, inflections) => span.map(_._1).updated(index, inflections(form)) }
  //     }
  //     (inflectedSpan ++ tokensSoFar, remainingAlignments)
  //   case (TemplateString(s), (tokensSoFar, remainingAlignments)) =>
  //     (s :: tokensSoFar, remainingAlignments)
  //   case (TemplatePro, (tokensSoFar, remainingAlignments)) =>
  //     System.err.println("Error: pronoun alignment not supported in templates")
  //     ("<pro>".lowerCase :: tokensSoFar, remainingAlignments)
  //   case (TemplateExt, (tokensSoFar, remainingAlignments)) =>
  //     System.err.println("Error: external word alignment not supported in templates")
  //     ("<ext>".lowerCase :: tokensSoFar, remainingAlignments)
  //   case (_, (tokensSoFar, Nil)) =>
  //     System.err.println("Error: not enough alignments for number of slots in template")
  //     ("<?>".lowerCase :: tokensSoFar, Nil)
  // }._1.toVector


  // TODO get rid of this if we can manage to put the extra stuff we computed into analysis only
  // and have analysis re-print as necessary
  // def makeOldTSV(
  //   ids: List[SentenceId],
  // ): String = {
  //   val sb = new StringBuilder
  //   for(id <- ids) {
  //     val sentence = id.tokens
  //     sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
  //     val sortedQAPairData = {
  //       val qaPairs = for {
  //         HITInfo(genHIT, genAssignments) <- allGenInfos
  //         if genHIT.prompt.id == id
  //         genAssignment <- genAssignments
  //         chosenValInfo <- allValInfos.find(_.hit.prompt.sourceAssignmentId.equals(genAssignment.assignmentId)).toList
  //         (wqa, qaIndex) <- genAssignment.response.zipWithIndex
  //         valAnswers = chosenValInfo.assignments.map(a => a.response(qaIndex))
  //         valFeedback = chosenValInfo.assignments.map(a => a.feedback).filterNot(_.isEmpty)
  //       } yield (
  //         genAssignment.workerId,
  //         genHIT.prompt.keywords,
  //         wqa,
  //         valAnswers,
  //         valAnswers.map(valAnswer =>
  //           ValidationAnswer.render(sentence, valAnswer, genAssignment.response)
  //         ).mkString("\t"),
  //         valFeedback.mkString("\t")
  //       )
  //       qaPairs.sortBy(_._3.wordIndex)
  //     }
  //     for(
  //       (genWorkerId, keywords,
  //        WordedQAPair(keywordIndex, question, answerIndices),
  //        valAnswers, valAnswersString, valFeedback
  //       ) <- sortedQAPairData) {
  //       val questionTokens = tokenize(question).toVector
  //       val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, questionTokens) // q-s
  //       val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
  //       sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t${keywords.toVector.sorted.mkString(" ")}\t${genWorkerId}\t")
  //       sb.append(s"${sentence(keywordIndex)} ($keywordIndex)\t$qsAlignmentsString\t${questionTokens.mkString(" ")}\t${valAnswers.size + 1}\t")
  //       sb.append(Text.renderSpan(sentence, answerIndices) + s"\t$valAnswersString\t")
  //       sb.append(s"${answerIndices.mkString(" ")}\t")
  //       sb.append(valAnswers.map(_.getAnswer.map(_.indices.mkString(" ")).getOrElse("")).mkString("\t"))
  //       sb.append(s"\t$valFeedback")
  //       sb.append("\n")
  //     }
  //   }
  //   sb.toString
  // }

//   // the pred itself, discourse markers, negations, and auxiliaries we don't care about
//   def labelIsIrrelevant(l: String) = {
//     l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD") ||
//       l.contains("C-") || l.contains("R-") ||
//       l == "rel"// || l == "Support"
//   }

//   case class PrecisionRecall(
//     numPredicted: Double,
//     numGold: Double,
//     numCorrect: Double,
//     numCovered: Double) {
//     val precision = numCorrect / numPredicted
//     val recall = numCovered / numGold
//     val f1 = 2 * precision * recall / (precision + recall)

//     def statString = f"F1: $f1%.3f\tPrecision: $precision%.3f\tRecall: $recall%.3f"

//     def aggregate(other: PrecisionRecall) = PrecisionRecall(
//       numPredicted + other.numPredicted,
//       numGold + other.numGold,
//       numCorrect + other.numCorrect,
//       numCovered + other.numCovered)
//   }
//   object PrecisionRecall {
//     val zero = PrecisionRecall(0, 0, 0, 0)
//   }

//   def statString(pr: PrecisionRecall) = pr.statString

//   def histogramString(hist: Scorer[Int, Int]): String = {
//     val vec = (0 to hist.keyIterator.max).map(hist.get).toVector
//     val max = vec.max
//     val scaleMax = 50.0
//     val scaleFactor = scaleMax / max
//     def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
//     def pounds(n: Int) = "#" * n
//     vec.zipWithIndex
//       .map { case (n, i) => f"$i%3d|${pounds(scale(n))}%s $n%d"}
//       .mkString("\n")
//   }

//   // assumes nonempty span ... checks it though
//   def getOffsetAndSpan(reference: Seq[String], span: Set[Int]) = {

//     import cats._
//     import cats.data._
//     import cats.implicits._

//     if(span.isEmpty) {
//       System.err.println("Identifying offset of empty span for reference:\n" + Text.render(reference))
//     }
//     if(span.exists(i => i < 0 || i >= reference.size)) {
//       System.err.println("Identifying offset of span containing indices outside of reference:\n" +
//                            Text.render(reference) + "\n" +
//                            span.mkString(" "))
//     }

//     @Lenses case class OffsetState(curOffset: Int, inSpan: Boolean, beginOffset: Int, phrase: String)
//     type ST[A] = State[OffsetState, A]
//     val firstWord = span.min
//     val lastWord = span.max
//     def possiblyAddToPhrase(text: String) =
//       State.modify(OffsetState.curOffset.modify(_ + text.length)) >>
//         State.modify(s =>
//           if(s.inSpan) OffsetState.phrase.modify(_ + text)(s) else s
//         )
//     def emitToken(token: String, index: Int): ST[String] = {
//       val normalizedToken = Text.normalizeToken(token)
//       for {
//         _ <- State.modify(if(index == firstWord) OffsetState.inSpan.set(true) else identity[OffsetState])
//         _ <- State.modify(if(index == firstWord) (s: OffsetState) => OffsetState.beginOffset.set(s.curOffset)(s)
//                           else identity[OffsetState])
//         _ <- possiblyAddToPhrase(normalizedToken)
//         _ <- State.modify(if(index == lastWord) OffsetState.inSpan.set(false) else identity[OffsetState])
//       } yield normalizedToken
//     }

//     val OffsetState(_, _, begin, phrase) = Text.renderM[(String, Int), List, ST, String](
//       reference.zipWithIndex.toList,
//       _._1,
//       _ => emitToken(" ", -1),
//       Function.tupled(emitToken)
//     ).runS(OffsetState(0, false, -1, "")).value

//     val sentence = Text.render(reference)
//     val reproPhrase = sentence.substring(begin, math.min(begin + phrase.length, sentence.length))
//     if(reproPhrase != phrase) {
//       System.err.println(
//         s"Problem for sentence\n$sentence \nGiven answer:\n$phrase \nRepro answer:\n$reproPhrase")
//     }

//     (begin, phrase)
//   }

//   /* do not use any of these above or outside the console since they become outdated */

//   lazy val alignedInfos: Map[
//     SentenceId,
//     Map[GenerationPrompt,
//         Map[Assignment[List[WordedQAPair]],
//             List[HITInfo[ValidationPrompt, List[ValidationAnswer]]]]]
//   ] = {
//     val genInfos = allGenInfos
//     val valInfos = allValInfos

//     allIds.map { id =>
//       id -> allPrompts.filter(_.id == id).map { prompt =>
//         prompt -> genInfos.filter(_.hit.prompt == prompt).flatMap(_.assignments).map { genAssignment =>
//           genAssignment -> valInfos.filter(_.hit.prompt.sourceAssignmentId == genAssignment.assignmentId)
//         }.toMap
//       }.toMap
//     }.toMap
//   }

//   lazy val alignedQAs: Map[SentenceId, Map[WordedQAPair, List[ValidationAnswer]]] = {
//     val genInfosByPrompt = allGenInfos.groupBy(_.hit.prompt)
//     val valInfosByGenAssignment = allValInfos.groupBy(_.hit.prompt.sourceAssignmentId)
//     allPrompts.groupBy(_.id).filter(p => allIds.contains(p._1)).flatMap { case (id, prompts) =>
//       val qaToAnswers = for {
//         prompt <- prompts
//         genInfo <- genInfosByPrompt.get(prompt).toList.flatten
//         assignment <- genInfo.assignments
//         validationResponses = for {
//           valInfo <- valInfosByGenAssignment.get(assignment.assignmentId).toList.flatten
//           assignment <- valInfo.assignments
//         } yield assignment.response
//         pair <- assignment.response.zip(validationResponses.transpose)
//       } yield pair
//       qaToAnswers.onlyIf(_.nonEmpty).map(id -> _.toMap)
//     }.toMap
//   }

//   lazy val validQAs: Map[SentenceId, Map[WordedQAPair, List[Set[Int]]]] = {
//     alignedQAs.map { case (id, qasToAnswers) =>
//       id -> qasToAnswers.flatMap { case (wqa, vAnswers) =>
//         vAnswers
//           .onlyIf(_.forall(_.isAnswer))
//           .map(_.flatMap(_.getAnswer).map(_.indices))
//           .map(wqa -> _)
//       }.toMap
//     }.toMap
//   }

//   def renderQAs(id: SentenceId, qas: Map[WordedQAPair, List[Set[Int]]]) = {
//     val sentence = id.tokens
//     Text.render(sentence) + "\n" +
//       qas.map { case (WordedQAPair(kwIndex, question, answerIndices), valAnswers) =>
//         val answerStrings = (answerIndices :: valAnswers).map(Text.renderSpan(sentence, _)).mkString(" \t| ")
//         s"\t$question --> \t$answerStrings"
//       }.mkString("\n")
//   }

//   def getExternalVocabulary(id: SentenceId, qas: List[SourcedQA[SentenceID]]) = {
//     val tokens = id.tokens
//     qas.flatMap { sqa =>
//       val qTokens = tokenize(sqa.question.toLowerCase).toVector
//       qTokens.indices
//         .filterNot(getAlignedQuestionIndices(tokens, qTokens))
//         .map(qTokens.apply _)
//     }.toList
//   }

//   def delexicalizeQuestion(id: SentenceId, question: String) = {
//     val qTokens = tokenize(question)
//     val sentenceTokens = id.tokens
//     val alignedQIs = getAlignedQuestionIndices(sentenceTokens, qTokens.toVector)
//     val posTagged = posTag(qTokens)
//     posTagged.zipWithIndex.map {
//       case (POSTaggedToken(token, pos), index) =>
//         if(alignedQIs.contains(index)) (token, s"*$pos")
//         else if(inflections.isCopulaVerb(token.lowerCase)) (token, "<be>")
//         else if(Inflections.doVerbs.contains(token.lowerCase)) (token, "<do>")
//         else (token, pos)
//     }
//   }

//   def sampleQAs(shuffleRand: Random, proportionQAsToSample: Double = 0.2, sentenceToQAs: Map[SentenceId, List[SourcedQA[SentenceID]]], n: Int) = {
//     def takeQAs(sentences: List[SentenceId], qasDesired: Int): Map[SentenceId, List[SourcedQA[SentenceID]]] = sentences match {
//       case Nil => Map.empty[SentenceId, List[SourcedQA[SentenceID]]]
//       case _ if qasDesired <= 0 => Map.empty[SentenceId, List[SourcedQA[SentenceID]]]
//       case id :: remainingIds =>
//         val sentenceQAs = shuffleRand.shuffle(sentenceToQAs(id).toVector)
//         val numQAsToSample = math.min(qasDesired, (sentenceQAs.size * proportionQAsToSample).toInt)
//         val sampledQAs = sentenceQAs.take(numQAsToSample).toList
//         if(sampledQAs.nonEmpty) {
//           takeQAs(remainingIds, qasDesired - numQAsToSample) + (id -> sampledQAs)
//         } else {
//           takeQAs(remainingIds, qasDesired - numQAsToSample)
//         }
//     }
//     val randomlyOrderedSentences = shuffleRand.shuffle(sentenceToQAs.keys.toVector).toList
//     takeQAs(randomlyOrderedSentences, n)
//   }

//   lazy val allQAs = {
//     // not sure how exactly to import the `sequence` extension method...
//     import scalaz._
//     import Scalaz._
//     import scalaz.std.list._
//     val iter = for {
//       (_, promptToAssignmentMap) <- alignedInfos.iterator
//       (prompt, assignmentToInfos) <- promptToAssignmentMap.iterator
//       (assignment, infos) <- assignmentToInfos.iterator
//       answersByQA = infos.flatMap(_.assignments).map(_.response).transpose
//       ((wqa, valAnswers), qaIndex) <- assignment.response.zip(answersByQA).zipWithIndex
//     } yield SourcedQA[SentenceID](
//       QAPairId(prompt, assignment.workerId, assignment.assignmentId, qaIndex),
//       wqa,
//       valAnswers)
//     iter.toList
//   }

//   class CoordinationAnalysis[SID <: SentenceId](data: QAData) {
//     val conjunctions = Set("and", "or").map(_.lowerCase)
//     lazy val qasWithCoordination = data.sentenceToQAs.flatMap {
//       case (id, qas) =>
//         val newQAs = qas.filter(
//           sqa => sqa.questionTokens
//             .map(_.lowerCase).toSet
//             .intersect(conjunctions)
//             .nonEmpty)
//           (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
//     }

//     lazy val qasWithCoordWh = qasWithCoordination.flatMap {
//       case (id, qas) =>
//         val newQAs = qas.filter(
//           sqa => sqa.questionTokens
//             .map(_.lowerCase)
//             .sliding(3)
//             .filter(
//             w => (w.size > 2) && conjunctions.contains(w(1)) &&
//               (questionWords.contains(w(0)) || questionWords.contains(w(2))))
//             .nonEmpty)
//           (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
//     }

//     lazy val totalNumQAs = data.sentenceToQAs.flatMap(_._2).size
//     lazy val numQAsWithCoord = qasWithCoordination.flatMap(_._2).size
//     lazy val numQAsWithCoordWh = qasWithCoordWh.flatMap(_._2).size

//     lazy val report = f"""
// QAs with and/or: $numQAsWithCoord%d (${numQAsWithCoord * 100.0 / totalNumQAs}%.2f%%)
// QAs with and/or beside a WH: $numQAsWithCoordWh%d (${numQAsWithCoordWh * 100.0 / numQAsWithCoord}%.2f%%)
// """.trim
//   }

//   def sampleQAPairs(sqas: List[SourcedQA[SentenceID]], n: Int = 1) = {
//     val qasByPrompt = sqas.groupBy(_.id.prompt)
//     val qas = qasByPrompt.values.flatMap { promptQAs =>
//       val qasByAssignment = promptQAs.groupBy(_.id.assignmentId)
//       val sample = Random.shuffle(qasByAssignment.keys.toVector).take(n)
//       for {
//         assignmentId <- sample
//         sqa <- qasByAssignment(assignmentId)
//       } yield sqa
//     }
//     qas
//   }

//   object PTBAnalysis {

//     object PASAnalysis {

//       import nlpdata.datasets.propbank._
//       import nlpdata.datasets.nombank._

//       case class PredArg(pred: Predicate, arg: ArgumentSpan)
//       def getRelevantPredArgs(pas: PredicateArgumentStructure) = pas.arguments
//         .map(PredArg(pas.pred, _))
//         .filterNot(pa => labelIsIrrelevant(pa.arg.label))
//         .filterNot(pa => Inflections.auxiliaryVerbs.contains(pa.pred.head.token.lowerCase))
//         .filterNot(pa => pa.arg.words.contains(pa.pred.head))

//       // heuristic alignment, based on each single question-answer pair
//       def alignToPASSimple(
//         words: Vector[String],
//         sqas: List[SourcedQA[SentenceID]],
//         paStructures: List[PredicateArgumentStructure]) = {
//         val qas = sqas.map(_.wqa)
//         val predArgs = paStructures.flatMap(getRelevantPredArgs)
//         val alignedPAs = qas.map {
//           case WordedQAPair(_, question, answer) =>
//             val qWords = getWordsInQuestion(words, question)
//             def alignment(predSide: Set[Int], argSide: Set[Int]) = predArgs
//               .filter(pa => predSide.contains(pa.pred.head.index))
//               .map(pa => pa -> argSide.intersect(pa.arg.words.map(_.index).toSet).size.toDouble / argSide.union(pa.arg.words.map(_.index).toSet).size)
//               .filter(_._2 > 0)
//               .sortBy(-_._2)
//               .headOption
//             val bestAlignment = List(
//               alignment(qWords, answer),
//               alignment(answer, qWords)
//             ).flatten.sortBy(-_._2).map(_._1).headOption
//             bestAlignment
//         }
//         // collapse QAs redundantly aligned... TODO: try without collapsing
//         val pasCovered = alignedPAs.flatten.toSet
//         // val numPredictions = alignedPAs.filter(_ == None).size// + pasCovered.size // TODO why was I adding this?
//         // because I use qas.size below, I should be consistent here. don't really care about precision anyway...
//         val numPredictions = qas.size

//         val sentenceString = Text.render(words)
//         val missedDeps = predArgs.filterNot(pasCovered).mkString("\n")
//         // println(s"\n\n$sentenceString\nMissed deps:\n$missedDeps")

//         PrecisionRecall(
//           numPredicted = numPredictions,
//           numGold = predArgs.size,
//           numCorrect = pasCovered.size,
//           numCovered = pasCovered.size
//         )
//       }

//       case class PASAlignment(
//         allPAs: List[PredArg],
//         alignedQAs: Map[SourcedQA[SentenceID], Option[PredArg]],
//         stats: PrecisionRecall) {
//         def coveredDeps = alignedQAs.values.flatten.toSet
//         def missedDeps = {
//           val covered = coveredDeps
//           allPAs.filterNot(covered)
//         }
//       }

//       def alignToPASSmarter(
//         tokens: Vector[String],
//         qas: List[SourcedQA[SentenceID]],
//         paStructures: List[PredicateArgumentStructure]): PASAlignment = {

//         // println(s"Sentence:\n${Text.render(tokens)}")
//         def breakIntoContiguous(s: Set[Int]): List[Set[Int]] = {
//           if(s.isEmpty) Nil else {
//             val min = s.min
//             var max = s.min + 1
//             while(s.contains(max)) {
//               max = max + 1
//             }
//             val firstInterval = (min until max).toSet
//             firstInterval :: breakIntoContiguous(s -- firstInterval)
//           }
//         }
//         // println(s"Questions:\n${qas.map(_.question).mkString("\n")}")
//         val allContiguousSpans = qas.flatMap { sqa =>
//           val qSpan = getWordsInQuestion(tokens, sqa.question)
//                                         (qSpan :: sqa.answers).flatMap(breakIntoContiguous)
//         }
//         // println(s"Contiguous spans:\n${allContiguousSpans.map(Text.renderSpan(tokens, _)).mkString("\n")}")
//         val minimalContiguousSpans = allContiguousSpans.filter(span =>
//           !allContiguousSpans.exists(otherSpan =>
//             otherSpan.subsetOf(span) && !span.subsetOf(otherSpan)
//           )
//         ).toSet
//         val minimalSpanQuestionAppearanceCounts = Scorer[Set[Int], Int](
//           qas.flatMap { qa =>
//             val qSpan = getWordsInQuestion(tokens, qa.question)
//             breakIntoContiguous(qSpan).filter(minimalContiguousSpans.contains)
//           }
//         )
//         // println(s"Minimal contiguous spans:\n${minimalContiguousSpans.map(Text.renderSpan(tokens, _)).mkString("\n")}")
//         val minimalSpanAllAppearanceCounts = Scorer[Set[Int], Int](
//           allContiguousSpans.filter(minimalContiguousSpans.contains)
//         )
//         val spansByPredicateness = {
//           val spanVec = minimalContiguousSpans.toVector
//           spanVec.zip(spanVec.map(s => minimalSpanQuestionAppearanceCounts(s) / minimalSpanAllAppearanceCounts(s)))
//             .sortBy(-_._2)
//             .map(_._1)
//         }
//         // println(s"Spans by salience:\n${spansByPredicateness.map(Text.renderSpan(tokens, _)).mkString("\n")}")

//         val allPredArgs = paStructures.flatMap(getRelevantPredArgs)
//         val alignedQAs = qas.map { sqa =>
//           // println(s"QA Pair:\t${sqa.question}\t${sqa.answers.map(Text.renderSpan(tokens, _))}")
//           val questionWords = getWordsInQuestion(tokens, sqa.question)
//           val questionPAs = for {
//             qNode <- spansByPredicateness.filter(_.subsetOf(questionWords))
//             pa @ PredArg(pred, arg) <- allPredArgs
//             .sortBy { pa =>
//               val argSpan = pa.arg.words.map(_.index).toSet
//                 -1.0 * sqa.answers.map(a => a.intersect(argSpan).size.toDouble / a.union(argSpan).size).mean
//             }
//             if qNode.contains(pred.head.index)
//             argSpan = arg.words.map(_.index).toSet
//             if(sqa.answers.filter(a => a.intersect(argSpan).nonEmpty)).size >= 1
//           } yield pa
//           val predArgOpt = questionPAs.headOption.orElse {
//             val answerPAs = for {
//               qNode <- spansByPredicateness.filter(_.subsetOf(questionWords))
//               pa @ PredArg(pred, arg) <- allPredArgs
//               argSpan = arg.words.map(_.index).toSet
//               if qNode.subsetOf(argSpan)
//               if(sqa.answers.filter(a => a.contains(pred.head.index))).size > 1
//             } yield pa
//             answerPAs.headOption
//           }
//           // println(s"PredArg alignment: $predArgOpt")
//           sqa -> predArgOpt
//         }.toMap
//         val numQAsAligned = alignedQAs.values.flatten.size
//         val pasCovered = alignedQAs.values.flatten.toSet
//         val numPredictions = qas.size
//         // println(s"PAs covered: $pasCovered")

//         val missedDeps = allPredArgs.filterNot(pasCovered)

//         val pr = PrecisionRecall(
//           numPredicted = numPredictions,
//           numGold = allPredArgs.size,
//           numCorrect = numQAsAligned,
//           numCovered = pasCovered.size)

//         PASAlignment(allPredArgs, alignedQAs, pr)
//       }

//       def alignToPAS(
//         tokens: Vector[String],
//         qas: List[SourcedQA[SentenceID]],
//         paStructures: List[PredicateArgumentStructure]
//       ) = alignToPASSmarter(tokens, qas, paStructures)

//       import nlpdata.datasets.propbank._

//       def propBankPR(path: PropBankSentencePath, tokens: Vector[String], qas: List[SourcedQA[SentenceID]]) = {
//         val pbSentence = PropBank.getSentence(path).get
//         val paStructures = pbSentence.predicateArgumentStructures
//         alignToPAS(tokens, qas, paStructures)
//       }

//       lazy val numPropBankSentences = {
//         val pbPaths = for {
//           PTBSentenceId(path) <- ptbData.sentenceToQAs.keys.iterator
//           pbPath <- PropBank.ptbToPropBankSentencePath(path).toOption.iterator
//         } yield pbPath
//         pbPaths.toSet.size
//       }

//       def allPropBankPRs(n: Int = 1) = {
//         val res = for {
//           (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
//           pbPath <- PropBank.ptbToPropBankSentencePath(path).toOption.iterator
//           tokens = id.tokens
//           sampledQAs = sampleQAPairs(qas, n)
//         } yield propBankPR(pbPath, tokens, sampledQAs.toList).stats
//         res.toList
//       }

//       lazy val pbRecalls = (1 to 5).map(i => List.fill(6 - i)(allPropBankPRs(i).reduce(_ aggregate _).recall))
//       lazy val pbRecallDists = pbRecalls.map(r => (r.mean, r.stdevSample))
//       lazy val pbRecallReport = s"PropBank:\nNumber of sentences: $numPropBankSentences\n" + pbRecallDists.zip(1 to 5)
//         .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
//         .mkString("\n")

//       import nlpdata.datasets.nombank._

//       def nomBankPR(path: PTBSentencePath, tokens: Vector[String], qas: List[SourcedQA[SentenceID]]) = {
//         val pas = NomBank.getPredArgStructuresReindexed(path).get
//         alignToPAS(tokens, qas, pas)
//       }

//       def allNomBankPRs(n: Int = 1) = {
//         val res = for {
//           (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
//           tokens = id.tokens
//           sampledQAs = sampleQAPairs(qas, n)
//         } yield nomBankPR(path, tokens, sampledQAs.toList).stats
//         res.toList
//       }.toList

//       lazy val numNomBankSentences = ptbData.sentenceToQAs.keys.size

//       lazy val nbRecalls = (1 to 5).map(i => List.fill(6 - i)(allNomBankPRs(i).reduce(_ aggregate _).recall))
//       lazy val nbRecallDists = nbRecalls.map(r => (r.mean, r.stdevSample))
//       lazy val nbRecallReport = s"NomBank:\nNumber of sentences: $numNomBankSentences\n" + nbRecallDists.zip(1 to 5)
//         .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
//         .mkString("\n")

//       import nlpdata.datasets.qasrl._
//       val qasrl = QASRL.getQASRL.get

//       // assumes path is stored
//       def qasrlPR(path: PTBSentencePath, tokens: Vector[String], qas: List[SourcedQA[SentenceID]]) = {
//         val qasrlSentence = qasrl(path)
//         alignToPAS(tokens, qas, qasrlSentence.predicateArgumentStructures)
//       }

//       lazy val numQASRLSentences = ptbData.sentenceToQAs.keys
//         .collect { case PTBSentenceId(path) => path }
//         .filter(qasrl.keySet.contains)
//         .size

//       def allQASRLPRs(n: Int = 1) = {
//         val res = for {
//           (id @ PTBSentenceId(path), qas) <- ptbData.sentenceToQAs.iterator
//           if qasrl.keySet.contains(path)
//           tokens = id.tokens
//           sampledQAs = sampleQAPairs(qas, n)
//         } yield qasrlPR(path, tokens, sampledQAs.toList).stats
//         res.toList
//       }

//       lazy val qasrlRecalls = (1 to 5).map(i => List.fill(6 - i)(allQASRLPRs(i).reduce(_ aggregate _).recall))
//       lazy val qasrlRecallDists = qasrlRecalls.map(r => (r.mean, r.stdevSample))
//       lazy val qasrlRecallReport = s"QA-SRL:\nNumber of sentences: $numQASRLSentences\n" + qasrlRecallDists.zip(1 to 5)
//         .map { case ((mean, stdev), n) => f"$n%d annotators: $mean%.4f ± $stdev%.4f" }
//         .mkString("\n")

//       def writeMissedDeps = {
//         val sb = new StringBuilder
//         val shuffleRand = new Random(821569L)
//         val shuffledSentences = shuffleRand.shuffle(ptbData.sentenceToQAs.keys.toVector)
//         for (id @ PTBSentenceId(path) <- shuffledSentences; if qasrl.keySet.contains(path)) {
//           val qas = ptbData.sentenceToQAs(id)
//           val tokens = id.tokens

//           val pbAlignmentOpt = PropBank.ptbToPropBankSentencePath(path)
//             .toOption.map(propBankPR(_, tokens, qas))
//           val nbAlignment = nomBankPR(path, tokens, qas)
//           val qasrlAlignment = qasrlPR(path, tokens, qas)

//           def addPA(pa: PredArg): Unit = pa match { case PredArg(pred, arg) =>
//             sb.append(s"\t${pred.head.token} (${pred.head.index}) --" + arg.label + "-> ")
//             sb.append(Text.render(arg.words.map(_.token)) + "\n")

//             // find relevant QA pairs
//             val relevantQAs = qas.filter { qa =>
//               val qWordsFromSentence = getWordsInQuestion(tokens, qa.question)
//               val allQAIndices = qWordsFromSentence ++ qa.answers.reduce(_ union _)
//               val relevantToPred = (
//                 allQAIndices.contains(pred.head.index) ||
//                   inflections.getAllForms(pred.head.token.lowerCase).map(_.toString)
//                   .exists(qa.question.toLowerCase.contains))
//               val relevantToArg = (
//                 allQAIndices.intersect(arg.words.map(_.index).toSet).nonEmpty ||
//                   qa.question.toLowerCase.contains(
//                     Text.renderSpan(tokens, arg.words.map(_.index).toSet).toLowerCase
//                   ))
//               relevantToPred & relevantToArg
//             }

//             relevantQAs.foreach { qa =>
//               val answers = qa.answers.map(Text.renderSpan(tokens, _)).distinct.mkString(" / ")
//               sb.append(s"|\t${qa.question}\t$answers\t")
//               pbAlignmentOpt.flatMap(_.alignedQAs(qa)) match {
//                 case None => sb.append("No PB alignment")
//                 case Some(PredArg(p, a)) =>
//                   sb.append(s"${p.head.token} (${p.head.index}) --" + a.label + "-> ")
//                   sb.append(Text.render(a.words.map(_.token)))
//               }
//               sb.append("\t")
//               nbAlignment.alignedQAs(qa) match {
//                 case None => sb.append("No NB alignment")
//                 case Some(PredArg(p, a)) =>
//                   sb.append(s"${p.head.token} (${p.head.index}) --" + a.label + "-> ")
//                   sb.append(Text.render(a.words.map(_.token)))
//               }
//               sb.append("\n")
//             }
//           }

//           if(pbAlignmentOpt.fold(false)(_.missedDeps.nonEmpty) || nbAlignment.missedDeps.nonEmpty || qasrlAlignment.missedDeps.nonEmpty) {
//             sb.append("==\t" + Text.render(tokens) + "\n")
//             pbAlignmentOpt.fold[Unit](sb.append("--\tNo PropBank data\n")) { pbAlignment =>
//               sb.append("--\tPropBank dependencies missed:\n")
//               pbAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
//             }
//             sb.append("--\tNomBank dependencies missed:\n")
//             nbAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
//             sb.append("--\tQA-SRL dependencies missed:\n")
//             qasrlAlignment.missedDeps.sortBy(_.pred.head.index).foreach(addPA)
//           }
//         }
//         val fileString = sb.toString
//         // saveDataFile(experimentName, "pbnb-missed-deps.tsv", fileString)
//         saveDataFile(experimentName, "qasrl-missed-deps.tsv", fileString)
//       }
//     }

//     // qasrl comparison too

//     // Constituency

//     class ConstituencyAnalysis(theseQAs: Map[SentenceId, List[SourcedQA[SentenceID]]]) {
//       // pct of answers that are spans
//       sealed trait SpanClassification {
//         def coordSibling: Boolean
//       }
//       case class SingleWord(coordSibling: Boolean = false) extends SpanClassification
//       case class SubNPSpan(coordSibling: Boolean = false) extends SpanClassification
//       case class ExactSpan(symbol: String, coordSibling: Boolean = false) extends SpanClassification
//       case object NonSpan extends SpanClassification {
//         override def coordSibling = false
//       }

//       def getSubtrees(tree: SyntaxTree, coordSibling: Boolean): List[(SyntaxTree, Boolean)] = tree match {
//         case leaf @ SyntaxTreeLeaf(_) => List((leaf, coordSibling))
//         case node @ SyntaxTreeNode(_, children) =>
//           val coordChild = children.collect {
//             case SyntaxTreeLeaf(word) => word.pos
//           }.contains("CC")
//                     (node, coordSibling) :: children
//             .map((_, coordChild))
//             .flatMap(Function.tupled(getSubtrees))
//       }

//       def classifySpan(subtrees: List[(SyntaxTree, Boolean)], span: Set[Int]): SpanClassification = {
//         val beginSubtrees = subtrees.filter(_._1.beginIndex == span.min).toSet
//         val endSubtrees = subtrees.filter(_._1.endIndex == span.max).toSet

//         (beginSubtrees intersect endSubtrees).headOption.map {
//           case (SyntaxTreeLeaf(w), coordSibling) =>
//             SingleWord(coordSibling)
//           case (SyntaxTreeNode(label, _), coordSibling) => ExactSpan(label, coordSibling)
//         }.orElse {
//           subtrees.collect { case (node @ SyntaxTreeNode(_, _), coordSibling) => (node, coordSibling) }
//             .filter(pair => pair._1.label.contains("NP") && pair._1.depth == 1)
//             .find(pair => pair._1.beginIndex <= span.min && pair._1.endIndex >= span.max)
//             .map(p => SubNPSpan(p._2))
//         }.getOrElse {
//           NonSpan
//         }
//       }

//       lazy val allSpanClassifications = theseQAs.iterator.collect { case (PTBSentenceId(path), sqas) =>
//         val sentence = PTB.getSentence(path).get
//         val answerSpans = sqas.flatMap(_.answers)
//         val subtrees = getSubtrees(sentence.syntaxTree, false)
//         answerSpans.map(classifySpan(subtrees, _))
//       }.flatten.toList

//       lazy val spanClassificationCounts = Scorer[SpanClassification, Int](allSpanClassifications.iterator)

//       lazy val sortedSpanClassifications = spanClassificationCounts.iterator.toVector.sortBy(-_._2)

//       lazy val totalNumAnswerSpans = spanClassificationCounts.sum

//       def constituencyReport(spanClass: SpanClassification, count: Int) =
//         f"${spanClass.toString.takeWhile(_ != '(')}%s\t$count%d\t(${count * 100.0 / totalNumAnswerSpans}%.2f%%)"

//       lazy val coordinationConstituencyReport = {
//         val coordSiblingCount = spanClassificationCounts.sumIf(_.coordSibling)
//         val notCoordSiblingCount = spanClassificationCounts.sumIf(!_.coordSibling)
//         def fineGrainedReport(spanClass: SpanClassification) = {
//           val count = spanClassificationCounts(spanClass)
//           f"$spanClass%s\t$count%d\t(${count * 100.0 / totalNumAnswerSpans}%.2f%%)"
//         }
//         val overallReport = f"Coordinator as sibling:\t(${pctString(coordSiblingCount, totalNumAnswerSpans)}%s)\n" +
//           f"No coordinator as sibling:\t(${pctString(notCoordSiblingCount, totalNumAnswerSpans)}%s)\n"
//         val someReports = List(SingleWord(true), SingleWord(false), SubNPSpan(true), SubNPSpan(false), NonSpan).map(fineGrainedReport)
//         val exactSpanReports = {
//           val exactMatchCoordSiblingCount = spanClassificationCounts.sumIf {
//             case ExactSpan(_, cs) => cs
//             case _ => false
//           }
//           val exactMatchNoCoordSiblingCount = spanClassificationCounts.sumIf {
//             case ExactSpan(_, cs) => !cs
//             case _ => false
//           }
//           "Coord sibling: " + constituencyReport(ExactSpan("", true), exactMatchCoordSiblingCount) + "\n" +
//             "No coord sibling: " + constituencyReport(ExactSpan("", false /* doesn't actually matter*/), exactMatchNoCoordSiblingCount)
//         }
//         overallReport + someReports.mkString("\n") + "\n" + exactSpanReports
//       }

//       lazy val fullConstituencyReport = {
//         val singleWordCount = spanClassificationCounts.sumIf {
//           case SingleWord(_) => true
//           case _ => false
//         }
//         val subNPCount = spanClassificationCounts.sumIf {
//           case SubNPSpan(_) => true
//           case _ => false
//         }
//         val exactMatchCount = spanClassificationCounts.sumIf {
//           case ExactSpan(_, _) => true
//           case _ => false
//         }
//         val nonSpanCount = spanClassificationCounts(NonSpan)
//         "Constituency:\n" +
//           constituencyReport(SingleWord(false), singleWordCount) + "\n" +
//           constituencyReport(SubNPSpan(false), subNPCount) + "\n" +
//           constituencyReport(ExactSpan("", false), exactMatchCount) + "\n" +
//           constituencyReport(NonSpan, nonSpanCount)
//       }
//     }

//     lazy val generalConstituencyAnalysis = new ConstituencyAnalysis(ptbData.sentenceToQAs)

//     lazy val ptbCoordinationAnalysis = new CoordinationAnalysis(ptbData)
//     lazy val coordinationConstituency = new ConstituencyAnalysis(ptbCoordinationAnalysis.qasWithCoordWh)

//     lazy val nonStartWhQAs = ptbData.sentenceToQAs.flatMap {
//       case (id, qas) =>
//         val newQAs = qas.filter(sqa => !questionWords.contains(sqa.questionTokens.head))
//                                (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
//     }
//     lazy val nonStartWhConstituencyAnalysis = new ConstituencyAnalysis(nonStartWhQAs)

//     lazy val fullReport: String = {
//       val pasAlignment = s"PAS alignment:\n" +
//         PASAnalysis.pbRecallReport + "\n" +
//         PASAnalysis.nbRecallReport + "\n" +
//         PASAnalysis.qasrlRecallReport + "\n"
//       val genConstituency = "\nGeneral constituency analysis:\n" +
//         generalConstituencyAnalysis.fullConstituencyReport + "\n"
//       val nonStartWhConstituency = "\nConstituency analysis for non-start WH-words:\n" +
//         nonStartWhConstituencyAnalysis.fullConstituencyReport + "\n"
//       val coordReport = "\nCoordination analysis:\n" +
//         ptbCoordinationAnalysis.report + "\n"
//       val coordConstituency = "Constituency analysis for coordinated answers:\n" +
//         "Coordination-specific:\n " + coordinationConstituency.coordinationConstituencyReport + "\n"
//       coordinationConstituency.fullConstituencyReport + "\n"
//       pasAlignment + genConstituency + nonStartWhConstituency + coordReport + coordConstituency
//     }

//     def makeTSV(thisData: QAData): String = {
//       val sb = new StringBuilder
//       for((id, sqas) <- thisData.sentenceToQAs.iterator) {
//         val sentence = id.tokens
//         sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
//         for(sqa <- sqas.iterator) {
//           val answerStrings = sqa.answers.map(Text.renderSpan(sentence, _))
//           val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
//           val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
//           sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
//           sb.append(sqa.id.prompt.keywords.toVector.sorted.mkString(" ") + "\t")
//           sb.append(s"${sqa.id.workerId}\t")
//           sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
//           sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
//           sb.append(sqa.answers.size + "\t")
//           sb.append(answerStrings.mkString("\t") + "\t")
//           sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
//           sb.append("\n")
//         }
//       }
//       sb.toString
//     }

//     def writePTBTSVs = {
//       val trainData = ptbData.filterBySentence(isTrain)
//       println("PTB train sentences: " + trainData.sentenceToQAs.size)
//       val devData = ptbData.filterBySentence(isDev)
//       println("PTB dev sentences: " + devData.sentenceToQAs.size)
//       val testData = ptbData.filterBySentence(isTest)
//       println("PTB test sentences: " + testData.sentenceToQAs.size)
//       val amrData = ptbData.filterBySentence(ptb100ForAMR.contains)
//       println("PTB AMR sentences: " + amrData.sentenceToQAs.size)
//       saveDataFile(experimentName, "ptb-train.tsv", makeTSV(trainData))
//       saveDataFile(experimentName, "ptb-dev.tsv", makeTSV(devData))
//       saveDataFile(experimentName, "ptb-test.tsv", makeTSV(testData))
//       saveDataFile(experimentName, "ptb-amr.tsv", makeTSV(amrData))
//     }
//   }

  // class CompleteAnalysis(data: QAData) {

//     lazy val coordinationAnalysis = new CoordinationAnalysis(data)

//     class RelationAnalysis(extractRelationWords: SourcedQA[SentenceID] => List[String]) {
//       lazy val relationWordCounts = Scorer.apply[String, Int](data.all.iterator.flatMap(extractRelationWords))

//       lazy val questionsWithRelationWords = data.filterByQA(sqa => extractRelationWords(sqa).nonEmpty)

//       lazy val questionsByRelation = {
//         var res = Map.empty[String, List[SourcedQA[SentenceID]]]
//         questionsWithRelationWords.all.iterator.foreach { sqa =>
//           extractRelationWords(sqa).foreach { w =>
//             res = res.updated(w, sqa :: res.get(w).getOrElse(Nil))
//           }
//         }
//         res
//       }

//       lazy val totalNumRelationWords = relationWordCounts.size
//       lazy val totalCountRelationWords = relationWordCounts.sum

//       lazy val orderedCountedRelationWords = relationWordCounts.iterator.toVector.sortBy(-_._2)
//       lazy val relationWordPrintables = orderedCountedRelationWords.map(p => s"${p._1} ${p._2} (${p._2 * 100.0 / totalCountRelationWords})")

//       // lazy val qaSample = sampleQAs(shuffleRand, )
//       def computeCoverageAtPercentile(topPercentile: Double) = {
//         val numPhrasesIncluded = math.round(totalNumRelationWords * topPercentile).toInt
//         val countCovered = orderedCountedRelationWords.take(numPhrasesIncluded).map(_._2).sum
//         s"${topPercentile * 100.0}% ($numPhrasesIncluded): ${pctString(countCovered, totalCountRelationWords)}"
//       }

//       def proportionQAsToSample = 0.2
//       // assumes we won't need to sample from all of the sentences
//       def sampleQuestions(shuffleRand: Random, n: Int) = {
//         def takeQAs(sentences: List[SentenceId], qasDesired: Int): Map[SentenceId, List[(List[String], SourcedQA[SentenceID])]] = sentences match {
//           case Nil => Map.empty[SentenceId, List[(List[String], SourcedQA[SentenceID])]]
//           case _ if qasDesired <= 0 => Map.empty[SentenceId, List[(List[String], SourcedQA[SentenceID])]]
//           case id :: remainingIds =>
//             val sentenceQAs = shuffleRand.shuffle(data.sentenceToQAs(id).toVector)
//             val numQAsToSample = math.min(qasDesired, (sentenceQAs.size * proportionQAsToSample).toInt)
//             val sampledQAs = sentenceQAs.map(sqa => (extractRelationWords(sqa), sqa))
//               .filter(_._1.nonEmpty)
//               .take(numQAsToSample)
//               .toList
//             if(sampledQAs.nonEmpty) {
//               takeQAs(remainingIds, qasDesired - numQAsToSample) + (id -> sampledQAs)
//             } else {
//               takeQAs(remainingIds, qasDesired - numQAsToSample)
//             }
//         }
//         val randomlyOrderedSentences = shuffleRand.shuffle(data.sentenceToQAs.keys.toVector).toList
//         takeQAs(randomlyOrderedSentences, n)
//       }

//       lazy val report = s"""
// Number of questions with relation phrase: ${pctString(questionsWithRelationWords.all.size, data.all.size)}
// Number of relation phrases: $totalNumRelationWords
// Number of relation phrase instances: $totalCountRelationWords
// Relation phrase question coverages at percentiles:
// ${computeCoverageAtPercentile(0.001)}
// ${computeCoverageAtPercentile(0.01)}
// ${computeCoverageAtPercentile(0.05)}
// ${computeCoverageAtPercentile(0.10)}
// ${computeCoverageAtPercentile(0.25)}
// ${computeCoverageAtPercentile(0.50)}
// ${computeCoverageAtPercentile(0.90)}

// Sample phrases:
// ${relationWordPrintables.take(100).mkString("\n")}
// """
//     }

//     val whDetWords = Set("what", "how", "which").map(_.lowerCase)
//     case class PhraseState(curPhrase: List[LowerCaseString], phrases: List[List[LowerCaseString]]) {
//       def finish(include: Boolean) = PhraseState(Nil, if(include && curPhrase.nonEmpty) curPhrase :: phrases else phrases)
//       def extend(token: String) = this.copy(curPhrase = token.lowerCase :: this.curPhrase)

//       def hasQWord: Boolean = curPhrase.exists(whDetWords.contains)
//       def hasNonQWord: Boolean = curPhrase.exists(!whDetWords.contains(_))
//     }

//     val interestingWords = Set("first", "last", "second", "third", "fourth")
//     def isInteresting(t: String) = !isReallyUninteresting(t) || interestingWords.contains(t)

//     // def getAllRelationWords(sqa: SourcedQA[SentenceID]) = {
//     //   val tokens = sqa.id.prompt.id.tokens
//     //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
//     //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
//     //   qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, acc) =>
//     //     val token = qTokens(index)
//     //     if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
//     //       acc.extend(token.lowerCase)
//     //     } else acc.finish(true)
//     //   }.phrases.map(_.mkString(" "))
//     // }
//     // def getWhRelationPhrases(sqa: SourcedQA[SentenceID]) = {
//     //   val tokens = sqa.id.prompt.id.tokens
//     //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
//     //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
//     //   val finalPhraseState = qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, phraseState) =>
//     //     val token = qTokens(index)
//     //     if(whDetWords.contains(token.lowerCase)) {
//     //       phraseState.extend(token)
//     //     } else if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
//     //       if(phraseState.hasQWord) phraseState.finish(phraseState.hasNonQWord).extend(token)
//     //       else phraseState.extend(token)
//     //     } else {
//     //       phraseState.finish(phraseState.hasQWord && phraseState.hasNonQWord)
//     //     }
//     //   }
//     //   finalPhraseState.finish(finalPhraseState.hasQWord && finalPhraseState.hasNonQWord).phrases.map(_.mkString(" "))
//     // }
//     // def getNonWhRelationWords(sqa: SourcedQA[SentenceID]) = {
//     //   val tokens = sqa.id.prompt.id.tokens
//     //   val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
//     //   val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
//     //   qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, acc) =>
//     //     val token = qTokens(index)
//     //     if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
//     //       acc.extend(token.lowerCase)
//     //     } else acc.finish(!whDetWords.contains(token.lowerCase))
//     //   }.phrases.map(_.mkString(" "))
//     // }

//     def getExternalPhrases(sqa: SourcedQA[SentenceID]) = {
//       val tokens = sqa.id.prompt.id.tokens
//       val qTokens = sqa.questionTokens.map(_.toLowerCase).toVector
//       val alignedIndices = getAlignedQuestionIndices(tokens, qTokens)
//       val finalPhraseState = qTokens.indices.foldRight(PhraseState(Nil, Nil)) { case (index, phraseState) =>
//         val token = qTokens(index)
//         if(whDetWords.contains(token.lowerCase)) {
//           phraseState.extend(token)
//         } else if(!alignedIndices.contains(index) && isInteresting(token) && !pronouns.contains(token.lowerCase)) {
//           if(phraseState.hasQWord) phraseState.finish(phraseState.hasNonQWord).extend(token)
//           else phraseState.extend(token)
//         } else {
//           phraseState.finish(phraseState.hasNonQWord)
//         }
//       }
//       finalPhraseState.finish(finalPhraseState.hasNonQWord).phrases
//     }
//     def getAllExternalPhrases(sqa: SourcedQA[SentenceID]) = {
//       getExternalPhrases(sqa).map(_.mkString(" "))
//     }
//     def getWhExternalPhrases(sqa: SourcedQA[SentenceID]) = {
//       getExternalPhrases(sqa).filter(_.exists(t => whDetWords.contains(t))).map(_.mkString(" "))
//     }
//     def getNonWhExternalPhrases(sqa: SourcedQA[SentenceID]) = {
//       getExternalPhrases(sqa).filter(!_.exists(t => whDetWords.contains(t))).map(_.mkString(" "))
//     }

//     val allRelationWordAnalysis = new RelationAnalysis(getAllExternalPhrases)
//     val whRelationWordAnalysis = new RelationAnalysis(getWhExternalPhrases)
//     val nonWhRelationWordAnalysis = new RelationAnalysis(getNonWhExternalPhrases)

//     // TODO put this in a more reasonable place
//     val lowerQWords = questionWords.map(_.lowerCase)

//     object DisagreementAnalysis {
//       // // explicit coref: delexicalized question of form
//       // val explicitCorefRegex = """WP <be> (?:\*?DT )?(?:\*?(?:NN|NNS|NNP|NNPS|PDT|PRP|FW)).*""".r
//       // lazy val explicitCorefQAs = validQAs.flatMap {
//       //   case (id, qas) =>
//       //     val newQAs = qas.filter { case (wqa, answers) =>
//       //       val template = delexicalizeQuestion(id, wqa.question).map(_._2 + " ").mkString
//       //       template match {
//       //         case explicitCorefRegex(_*) => true
//       //         case _ => false
//       //       }
//       //     }
//       //       (id -> newQAs).onlyIf(const(newQAs.nonEmpty))
//       // }.toMap

//       // implicit coref: nonoverlapping answers to same question
//       lazy val disagreementQAs = data.filterByQA { sqa =>
//         sqa.answers.reduce(_ intersect _).isEmpty
//       }

//       // lazy val sameAnswerQs = validQAs.flatMap {
//       //   case (id, qas) =>
//       //     val groupedQs = qas.iterator.map { case (wqa, answers) =>
//       //       (wqa, answers) -> answers.reduce(_ intersect _)
//       //     }.filter(_._2.nonEmpty).toVector.groupBy(_._2).map {
//       //       case (answer, qPairs) => answer -> qPairs.map(_._1).toSet.toVector
//       //     }.filter(_._2.size > 1).toMap
//       //     (id -> groupedQs).onlyIf(const(groupedQs.nonEmpty))
//       // }.toMap

//       lazy val report = s"""
// Number of questions with disagreeing answers: ${pctString(disagreementQAs.all.size, data.all.size)}
// """.trim
//     }

//     object ValidationStats {
//       case class SentenceInfo(assignmentInfos: List[AssignmentInfo]) {
//         def sqas = assignmentInfos.flatMap(_.sqas)
//         def id = sqas.head.id.prompt.id
//         def numQAs = sqas.size
//         def numValidQAs = sqas.filter(_.isValid).size
//         def numGoodQAs = sqas.filter(_.isGood).size
//         def cost = assignmentInfos.map(_.totalReward.toDouble).sum
//         def costPerToken = cost / id.tokens.size
//         def numKeywords = assignmentInfos.map(_.numKeywords).sum
//       }

//       case class AssignmentInfo(keywordInfos: List[KeywordInfo]) {
//         def sqas = keywordInfos.flatMap(_.sqas)
//         def prompt = sqas.head.id.prompt
//         def numQAs = sqas.size
//         def numValidForBonus = math.round(
//           sqas.map(_.validatorAnswers.filter(_.isAnswer).size).mean - 0.01
//         ).toInt
//         def numValidQAs = sqas.filter(_.isValid).size
//         def numGoodQAs = sqas.filter(_.isGood).size
//         def genReward = generationReward + (1 to (numValidQAs - prompt.keywords.size)).map(bonusFor).sum
//         def valReward = validationReward + (validationBonusPerQuestion * math.max(0, numQAs - 4))
//         def totalReward = genReward + valReward
//         def numKeywords = keywordInfos.size
//       }
//       case class KeywordInfo(sqas: List[SourcedQA[SentenceID]]) {
//         def prompt = sqas.head.id.prompt
//         def keywords = prompt.keywords
//         def numQAs = sqas.size
//         def numValidQAs = sqas.filter(_.isValid).size
//         def numGoodQAs = sqas.filter(_.isGood).size
//       }

//       lazy val sentenceInfos = data.sentenceToQAsUnfiltered.values.iterator.map { sentenceSQAs =>
//         SentenceInfo(
//           sentenceSQAs.groupBy(_.id.assignmentId).values.map { assignmentSQAs =>
//             AssignmentInfo(
//               assignmentSQAs.groupBy(_.wqa.wordIndex).values.map { sqas =>
//                 KeywordInfo(sqas.toList)
//               }.toList
//             )
//           }.toList
//         )
//       }.toList

//       lazy val assignmentInfos = sentenceInfos.flatMap(_.assignmentInfos)
//       lazy val keywordInfos = assignmentInfos.flatMap(_.keywordInfos)

//       sealed trait AgreementClass
//       case object BothInvalid extends AgreementClass
//       case object OneInvalid extends AgreementClass
//       case object BothRedundant extends AgreementClass
//       case object OneRedundant extends AgreementClass
//       case object BothWithOriginal extends AgreementClass
//       case object BothButNotOriginal extends AgreementClass
//       case object OneWithOriginal extends AgreementClass
//       case object NoIntersection extends AgreementClass
//       lazy val agreementClasses = data.allUnfiltered.map(sqa => (sqa, sqa.validatorAnswers)).collect {
//         case (sqa, List(InvalidQuestion, InvalidQuestion)) => BothInvalid
//         case (sqa, List(InvalidQuestion, _)) => OneInvalid
//         case (sqa, List(_, InvalidQuestion)) => OneInvalid
//         case (sqa, List(Redundant(_), Redundant(_))) => BothRedundant
//         case (sqa, List(_, Redundant(_))) => OneRedundant
//         case (sqa, List(Redundant(_), _)) => OneRedundant
//         case (sqa, List(Answer(a1), Answer(a2))) if (sqa.wqa.answer :: a1 :: a2 :: Nil).reduce(_ intersect _).nonEmpty => BothWithOriginal
//         case (sqa, List(Answer(a1), Answer(a2))) if (a1 :: a2 :: Nil).reduce(_ intersect _).nonEmpty => BothButNotOriginal
//         case (sqa, (List(Answer(a1), Answer(a2)))) if (
//           (a1 intersect sqa.wqa.answer).nonEmpty || (a2 intersect sqa.wqa.answer).nonEmpty
//         ) => OneWithOriginal
//         case (sqa, List(Answer(a1), Answer(a2))) if (
//           (sqa.wqa.answer ++ a1 ++ a2).size == (sqa.wqa.answer.size + a1.size + a2.size)
//         ) => NoIntersection
//       }

//       lazy val agClassCounts = Scorer[AgreementClass, Int](agreementClasses.iterator)

//       lazy val agClassHist: String = {
//         val keys = Vector(
//           BothInvalid, OneInvalid, BothRedundant, OneRedundant,
//           BothWithOriginal, BothButNotOriginal, OneWithOriginal, NoIntersection
//         )
//         val max = agClassCounts.max
//         val scaleMax = 50.0
//         val scaleFactor = scaleMax / max
//         def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
//         def pounds(n: Int) = "#" * n
//         keys.zip(keys.map(agClassCounts.apply))
//           .map { case (c, n) => f"$c%18s |${pounds(scale(n))}%s $n%d"}
//           .mkString("\n")
//       }


//       lazy val report = f"""
// Aggregate stats:
// Number of questions: ${sentenceInfos.map(_.numQAs).sum}%s
// Number of valid questions: ${pctString(sentenceInfos.map(_.numValidQAs).sum, sentenceInfos.map(_.numQAs).sum)}%s
// Number of good questions: ${pctString(sentenceInfos.map(_.numGoodQAs).sum, sentenceInfos.map(_.numValidQAs).sum)}%s
// Validator agreement class counts:
// $agClassHist

// Sentences:
// Number of sentences: ${sentenceInfos.size}%s
// Number of sentences with good QAs: ${pctString(data.sentenceToQAs.size, sentenceInfos.size)}%s
// Number of keywords per sentence: ${sentenceInfos.map(_.numKeywords).sum}%s
// Sentence costs: ${noSumDistString(sentenceInfos.map(_.cost))}%s
// Sentence cost per token: ${noSumDistString(sentenceInfos.map(_.costPerToken))}%s
// Number of questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numQAs))}%s
// Number of valid questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numValidQAs))}%s
// Number of good questions (per sentence): ${noSumDistString(sentenceInfos.map(_.numGoodQAs))}%s

// Assignments:
// Number of assignments: ${assignmentInfos.size}%s
// Assignment costs: ${noSumDistString(assignmentInfos.map(_.totalReward))}%s
// Number of questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numQAs))}%s
// Number of valid questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numValidQAs))}%s
// Number of good questions (per assignment): ${noSumDistString(assignmentInfos.map(_.numGoodQAs))}%s

// Keywords:
// Number of keywords: ${keywordInfos.size}%s
// Number of questions per keyword: ${noSumDistString(keywordInfos.map(_.numQAs))}%s
// Number of valid questions per keyword: ${noSumDistString(keywordInfos.map(_.numValidQAs))}%s
// Number of good questions per keyword: ${noSumDistString(keywordInfos.map(_.numGoodQAs))}%s
// Number of keywords missing: ${pctString(keywordInfos.filter(_.numValidQAs == 0).size, keywordInfos.size)}%s
// """.trim
//     }

//     def report: String = {
//       val sep = "\n" + ("=" * 20) + "\n"
//       println("Computing report...")
//       sep + "Question modeling:\n" + QuestionModeling.report + sep +
//         sep + "External word stats:\n" + allRelationWordAnalysis.report + sep +
//         sep + "Classifier relation word stats:\n" + whRelationWordAnalysis.report + sep +
//         sep + "Non-classifier relation word stats:\n" + nonWhRelationWordAnalysis.report + sep +
//         sep + "Annotator disagreement:\n" + DisagreementAnalysis.report + sep
//     }

//     def topRelationPhrases(n: Int): String =
//       allRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")
//     def topWhRelationPhrases(n: Int): String =
//       whRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")
//     def topNonWhRelationPhrases(n: Int): String =
//       nonWhRelationWordAnalysis.relationWordPrintables.take(n).mkString("\n")

//     // used to provide resources for submission
//     def printSentencesToAnnotateNICE = {
//       import data._
//       import scalaz._
//       import Scalaz._
//       import scala.language.higherKinds
//       type PrintingState[A] = State[List[String], A]
//       type Printing[A] = ListT[PrintingState, A]
//       def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
//       def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))

//       def niceTSV(allQAs: List[(SentenceId, List[SourcedQA[SentenceID]])]) = {
//         val printer = for {
//           _ <- append(s"Below are all question-answer pairs we collected for a sample of ${allQAs.size} sentences. (tab-separated; best viewed as a spreadsheet)")
//                      (id, qas) <- iter(allQAs)
//           sentenceTokens = id.tokens)
//           _ <- append("\n" + Text.render(sentenceTokens) + "\n")
//           sqa <- iter(qas)
//           _ <- append(s"${sqa.question}\t")
//           _ <- append(sqa.answers.map(a => Text.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
//         } yield ()
//         printer.run.exec(Nil).reverse.mkString
//       }

//       // saveDataFile(
//       //   experimentName,
//       //   "wh-relationQs.tsv",
//       //   relationTSV(whRelationWordAnalysis.sampleQuestions(new Random(7654326L), 1000)))
//       // saveDataFile(
//       //   experimentName,
//       //   "nonWh-relationQs.tsv",
//       //   relationTSV(nonWhRelationWordAnalysis.sampleQuestions(new Random(35265419L), 1000)))

//       // saveDataFile(
//       //   experimentName,
//       //   "disagreementQs.tsv",
//       //   niceTSV(sampleQAs(new Random(356127158L), 1.0, DisagreementAnalysis.disagreementQAs.sentenceToQAs, 1000)))

//       val fullSample = sampleQAs(new Random(23536289L), 1.0, data.sentenceToQAs, 500).toList
//       val trainSample = fullSample.filter(p => isTrain(p._1)).take(50)
//       val devSample = fullSample.filter(p => isDev(p._1)).take(50)

//       saveDataFile(
//         experimentName,
//         "train-sample.tsv",
//         niceTSV(trainSample))

//       saveDataFile(
//         experimentName,
//         "dev-sample.tsv",
//         niceTSV(devSample))

//       // saveDataFile(
//       //   experimentName,
//       //   "dev-sample.tsv",
//       //   niceTSV(
//       //     sampleQAs(
//       //       new Random(23536289L),
//       //       1.0,
//       //       data.sentenceToQAs.filter(p => isDev(p._1)),
//       //       50)))
//     }

//     def printSentencesToAnnotate = {
//       import data._
//       import scalaz._
//       import Scalaz._
//       import scala.language.higherKinds
//       type PrintingState[A] = State[List[String], A]
//       type Printing[A] = ListT[PrintingState, A]
//       def append(s: String): Printing[Unit] = State.modify[List[String]](s :: _).liftM[ListT]
//       def iter[A](l: List[A]): Printing[A] = ListT.fromList[PrintingState, A](State.state[List[String], List[A]](l))

//       def relationTSV(allQAs: Map[SentenceId, List[(List[String], SourcedQA[SentenceID])]]) = {
//         val printer = for {
//           _ <- append("\nCode\tPhrases\tQuestion\tOriginal Answer\tValidator Answers\n")
//                      (id, qas) <- iter(allQAs.toList)
//           sentenceTokens = id.tokens
//           _ <- append("==\t" + Text.render(sentenceTokens) + "\n")
//                      (phrases, (sqa)) <- iter(qas.toList)
//           _ <- append("\t" + phrases.mkString("; ") + s"\t${sqa.question}\t")
//           _ <- append(sqa.answers.map(a => Text.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
//         } yield ()
//         printer.run.exec(Nil).reverse.mkString
//       }

//       def generalTSV(allQAs: Map[SentenceId, List[SourcedQA[SentenceID]]]) = {
//         val printer = for {
//           _ <- append("\nCode\tQuestion\tOriginal Answer\tValidator Answers\n")
//                      (id, qas) <- iter(allQAs.toList)
//           sentenceTokens = id.tokens
//           _ <- append("==\t" + Text.render(sentenceTokens) + "\n")
//           sqa <- iter(qas)
//           _ <- append(s"\t${sqa.question}\t")
//           _ <- append(sqa.answers.map(a => Text.renderSpan(sentenceTokens, a)).mkString("\t") + "\n")
//         } yield ()
//         printer.run.exec(Nil).reverse.mkString
//       }

//       saveDataFile(
//         experimentName,
//         "wh-relationQs.tsv",
//         relationTSV(whRelationWordAnalysis.sampleQuestions(new Random(7654326L), 1000)))
//       saveDataFile(
//         experimentName,
//         "nonWh-relationQs.tsv",
//         relationTSV(nonWhRelationWordAnalysis.sampleQuestions(new Random(35265419L), 1000)))

//       saveDataFile(
//         experimentName,
//         "disagreementQs.tsv",
//         generalTSV(sampleQAs(new Random(356127158L), 1.0, DisagreementAnalysis.disagreementQAs.sentenceToQAs, 1000)))

//       saveDataFile(
//         experimentName,
//         "random-validQAs.tsv",
//         generalTSV(sampleQAs(new Random(23536289L), 0.2, data.sentenceToQAs, 1000)))
//     }

//     def makeTSV: String = {
//       import scalaz._
//       import Scalaz._
//       import scala.language.higherKinds
//       val sb = new StringBuilder
//       for((id, sqas) <- data.sentenceToQAs.iterator) {
//         val sentence = id.tokens
//         sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
//         for(sqa <- sqas.iterator) {
//           val answerStrings = sqa.answers.map(Text.renderSpan(sentence, _))
//           val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
//           val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
//           sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
//           sb.append(s"${sqa.id.workerId}\t")
//           sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
//           sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
//           sb.append(answerStrings.mkString("\t") + "\t")
//           sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
//           sb.append("\n")
//         }
//       }
//       sb.toString
//     }

//     lazy val sortedCountedQuestionPrefixes = {
//       val prefixCounts = Scorer[List[String], Int](
//         data.all.iterator
//           .map(_.questionTokens)
//           .map(_.map(_.toLowerCase))
//           .flatMap(_.inits.toList.init)
//       )
//       prefixCounts.iterator.toVector.sortBy(-_._2)
//     }

//     def printQuestionPrefixes(n: Int) = {
//       val sb = new StringBuilder
//       sb.append("Parent Category Label,Product Category,Level,Sales,Path\n")
//       sortedCountedQuestionPrefixes.take(n).foreach { case (tokens, count) =>
//         sb.append(List(tokens.last, tokens.mkString(" > "), tokens.size, count, 1).mkString(",") + "\n")
//       }
//       sortedCountedQuestionPrefixes.take(n).foreach { case (tokens, count) =>
//         sb.append(List(tokens.last, tokens.mkString(" > "), tokens.size, count, 203).mkString(",") + "\n")
//       }

//       saveDataFile(experimentName, "q-prefixes.tsv", sb.toString)
//     }
//   }


//   lazy val allData = new QAData(allQAs)
//   lazy val ptbData = allData.filterBySentence(_.isPTB)

//   lazy val allAnalysis = new CompleteAnalysis(allData)

//   lazy val ptbAnalysis = new CompleteAnalysis(ptbData)

//   lazy val allWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(_.isWiki))

//   lazy val trainWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isTrain(id)))
//   lazy val devWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isDev(id)))
//   lazy val testWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && isTest(id)))

//   lazy val trainDevWikiAnalysis = new CompleteAnalysis(allData.filterBySentence(id => id.isWiki && (isTrain(id) || isDev(id))))
//   lazy val trainDevWikiAndAllPTBAnalysis = new CompleteAnalysis(allData.filterBySentence(id => !(id.isWiki && isTest(id))))

//   // scatter : length / num QA pairs

//   object CoverageStats {

//     // def orderedPairsCovered(sentence: Vector[String], question: String, answerIndices: Set[Int]) = {
//     //   val pairs = for {
//     //     qi <- getWordsInQuestion(sentence, question)
//     //     ai <- answerIndices
//     //   } yield (math.min(qi, ai), math.max(qi, ai))
//     //   pairs.toSet
//     // }

//     // case class PairCoverage(
//     //   id: SentenceId,
//     //   pairs: Set[(Int, Int)]) {
//     //   def sentence = id.tokens
//     //   def numCovered = pairs.size
//     //   def numPossible = math.pow(sentence.size, 2).toInt
//     // }
//     // def nSamplePairCoverage(n: Int): Map[SentenceId, PairCoverage] = alignedInfos.map {
//     //   case (id, promptToAlignments) =>
//     //     val sentence = id.tokens
//     //     val wqas = sampleQAPairs(id, n)
//     //     val pairs = wqas.map {
//     //       case WordedQAPair(_, question, answer) => orderedPairsCovered(sentence, question, answer)
//     //     }.foldLeft(Set.empty[(Int, Int)])(_ union _)
//     //     id -> PairCoverage(id, pairs)
//     // }.toMap

//     // lazy val coverageCountsBySentence = {
//     //   val coverages = (1 to 5).map(nSamplePairCoverage).toList
//     //   allIds.map { id =>
//     //     id -> coverages.map(_(id)).map(_.numCovered)
//     //   }.toMap
//     // }

//     // def avgCoveragePercentages(n: Int = 20) = {
//     //   coverageCountsBySentence.values.toVector
//     //     .sortBy(_.last)
//     //     .take(n)
//     //     .map(counts => counts.map(_ * 100.0 / counts.last))
//     //     .toList.transpose.map(_.mean)
//     // }

//     // lazy val avgQAsPerKeywordByWorker = allGenInfos.flatMap(_.assignments).groupBy(_.workerId).map {
//     //   case (worker, as) => worker -> {
//     //     val nums = as.flatMap(_.response).groupBy(_.wordIndex).map(_._2.size).toList
//     //     (nums.mean, nums.size)
//     //   }
//     // }

//   }

//   lazy val actuallyEverythingReport = {
//     val sep = "\n" + ((("=" * 40) + "\n") * 2) + "\n"
//     sep + "All unfiltered data aggregate stats:\n" + allAnalysis.ValidationStats.report + sep +
//       sep + "PTB unfiltered data aggregate stats:\n" + ptbAnalysis.ValidationStats.report + sep +
//       sep + "Wiki unfiltered data aggregate stats:\n" + allWikiAnalysis.ValidationStats.report + sep +
//       sep + "Train Wiki data:\n" + trainWikiAnalysis.ValidationStats.report + sep +
//       sep + "Dev Wiki data:\n" + devWikiAnalysis.ValidationStats.report + sep +
//       sep + "Test Wiki data:\n" + testWikiAnalysis.ValidationStats.report + sep +
//       sep + "Penn Treebank specific analysis:\n" + PTBAnalysis.fullReport + sep +
//       sep + "Penn Treebank general analysis:\n" + ptbAnalysis.report + sep +
//       sep + "Train + Dev Wiki analysis:\n" + trainDevWikiAnalysis.report + sep
//   }

//   def printEverythingReport = saveDataFile(experimentName, "all-reports.txt", actuallyEverythingReport)

//   def printSampledQAs = trainDevWikiAnalysis.printSentencesToAnnotate

//   def printSampledQAsAllQAsForSentences = trainDevWikiAnalysis.printSentencesToAnnotateNICE

//   def printQGData = {
//     saveDataFile(experimentName, "train.tsv", trainWikiAnalysis.makeTSV)
//     saveDataFile(experimentName, "dev.tsv", devWikiAnalysis.makeTSV)
//     saveDataFile(experimentName, "test.tsv", testWikiAnalysis.makeTSV)
//   }

//   def printQAData = {
//     trainWikiAnalysis.writeAllSquadFormatted(
//       "squad-train.json",
//       Set("Nikola Tesla", "Oxygen", "Geology", "Genghis Khan", "Imperialism"))
//     devWikiAnalysis.writeAllSquadFormatted(
//       "squad-dev.json",
//       Set("Brain", "Emotion"))
//     testWikiAnalysis.writeAllSquadFormatted(
//       "squad-test.json",
//       Set("Architecture", "Middle Ages", "Avicenna", "Capacitor", "Martin Luther", "Steam engine"))
//   }

//   def printSRLFormattedData = {
//     trainWikiAnalysis.writeAllSRLFormatted(x => s"srl-train-$x.txt")
//     devWikiAnalysis.writeAllSRLFormatted(x => s"srl-dev-$x.txt")
//   }

//   // TODO get this information shit printed as fuck
//   // def printWorkerStats = {
//   //   // generation
//   //   val genSB = new StringBuilder
//   //   sb.append("assignmentId\thitId\tworkerId")
//   //   for(HITInfo(hit, assignments) <- allGenInfos) {
//   //     val sentence = hit.prompt.id.tokens
//   //     for(assignment <- assignments) {

//   //     }
//   //     sb.append("\t\t\t" + sentence.mkString(" ") + "\n")
//   //     for(sqa <- sqas.iterator) {
//   //       val answerStrings = sqa.answers.map(Text.renderSpan(sentence, _))
//   //       val questionSentenceAlignments = getQuestionSentenceAlignments(sentence, sqa.questionTokens.toVector) // q-s
//   //       val qsAlignmentsString = questionSentenceAlignments.map { case (q, s) => s"$q-$s" }.mkString(" ")
//   //       sb.append(s"${id.readableFileString}\t${id.readableSentenceIndex}\t")
//   //       sb.append(s"${sqa.id.workerId}\t")
//   //       sb.append(s"${sentence(sqa.wqa.wordIndex)} (${sqa.wqa.wordIndex})\t${qsAlignmentsString}\t")
//   //       sb.append(sqa.questionTokens.mkString(" ").capitalize + "\t")
//   //       sb.append(answerStrings.mkString("\t") + "\t")
//   //       sb.append(sqa.answers.map(_.mkString(" ")).mkString("\t"))
//   //       sb.append("\n")
//   //     }
//   //   }
//   //   sb.toString
//   // }
}


// class QuestionModeling(data: QAData) {
//   // def validQAsForNumQuestionWords(p: Int => Boolean) = theseValidQAs.map { case (id, qas) =>
//   //   val tokens = id.tokens
//   //   id -> qas.filter { case (wqa, answers) => p(getWordsInQuestion(tokens, wqa.question).size) }
//   // }.toMap

//   lazy val externalWordCounts = data.sentenceToQAs.iterator.flatMap(
//     Function.tupled(getExternalVocabulary)
//   ) <| Scorer.apply[String, Int]

//   // def externalWordReport(sum: Double)(word: String, count: Int) =
//   //   f"$word%s\t$count%d\t${count.toDouble / sum}%.4f"

//   // lazy val externalWordReports = externalWordCounts.iterator.toVector
//   //   .sortBy(-_._2)
//   //   .map(Function.tupled(externalWordReport(externalWordCounts.sum)))

//   // lazy val externalNonStopwordCounts = theseValidQAs.iterator.flatMap(
//   //   Function.tupled(getExternalVocabulary)
//   // ).filterNot(isReallyUninteresting) <| Scorer.apply[String, Int]

//   // lazy val externalNonStopwordReports = externalNonStopwordCounts.iterator.toVector
//   //   .sortBy(-_._2)
//   //   .map(Function.tupled(externalWordReport(externalNonStopwordCounts.sum)))

//   // lazy val allValidQuestions = theseValidQAs.map {
//   //   case (id, qaMap) => id -> qaMap.keys.map(wqa => posTag(tokenize(wqa.question)))
//   // }.toMap

//   // lazy val numValidQuestions = allValidQuestions.iterator.map(_._2.size).sum

//   // class NGramReport(tokenizedStrings: Iterator[List[String]]) {
//   //   lazy val prefixes = tokenizedStrings.flatMap(tokens =>
//   //     (tokens ++ List("<End>")).inits.filter(_.nonEmpty)
//   //   ) <| Scorer.apply[List[String], Int]

//   //   lazy val orderedPrefixes = prefixes.iterator.toVector.sortBy(-_._2)

//   //   def prefixReport(prefix: List[String], count: Int) =
//   //     f"${prefix.mkString(" ")}%s\t$count%d\t${count.toDouble / numValidQuestions}%.4f"

//   //   lazy val orderedPrefixReports = orderedPrefixes.map(Function.tupled(prefixReport))
//   // }

//   // lazy val questionNGrams = new NGramReport(allValidQuestions.iterator.flatMap(_._2).map(_.map(_.token.toLowerCase)))

//   // lazy val collapsedQuestions = allValidQuestions.map {
//   //   case (id, questions) => id -> questions.map { q =>
//   //     val alignedTokens = getAlignedQuestionIndices(id.tokens, q.map(_.token).toVector)
//   //     val collapsedTokens = q.zipWithIndex.foldRight(List.empty[POSTaggedToken]) { case ((posToken, index), acc) =>
//   //       if(alignedTokens.contains(index)) {
//   //         if(acc.headOption.fold(true)(_.token != "<>")) POSTaggedToken("<>", "<>") :: acc
//   //         else acc
//   //       } else posToken.copy(token = posToken.token.toLowerCase) :: acc
//   //     }
//   //     collapsedTokens
//   //   }
//   // }

//   // lazy val collapsedQuestionNGrams = new NGramReport(collapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

//   // lazy val auxCollapsedQuestions = collapsedQuestions.map {
//   //   case (id, cQuestions) => id -> cQuestions.map { tokens =>
//   //     tokens.map { case t =>
//   //       if(inflections.isCopulaVerb(t.token.lowerCase)) POSTaggedToken("<be>", "<be>")
//   //       else if(Inflections.doVerbs.contains(t.token.lowerCase)) POSTaggedToken("<do>", "<do>")
//   //       else t
//   //     }
//   //   }
//   // }

//   // lazy val auxCollapsedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.token)))

//   // lazy val delexicalizedQuestionNGrams = new NGramReport(auxCollapsedQuestions.iterator.flatMap(_._2).map(_.map(_.pos)))

//   // val determiners = Set("the", "a", "this")
//   // val pronouns = Set("i", "we", "you", "he", "she", "him", "her", "it", "something", "someone", "they", "them")
//   // val kindCats = Set("kind", "type", "types")
//   // val whCats = Set("year", "country", "part", "month", "day", "people", "nationality", "city", "place", "thing",
//   //                  "group", "event", "time", "number", "man", "things", "language", "person", "album", "position",
//   //                  "animal", "years", "state", "size", "color", "score", "percentage", "date", "gender", "countries",
//   //                  "direction", "organization", "level", "religion", "profession", "company", "job")
//   // val ofCats = Set("name", "title")
//   // val howCats = Set("long", "old")

//   // def writeTemplatedQuestions(filename: String) = {
//   //   val sb = new StringBuilder
//   //   for {
//   //     (id, qaPairToAnswers) <- theseValidQAs
//   //     sentenceTokens = posTag(id.tokens.toList)
//   //                            (wqa, answers) <- qaPairToAnswers
//   //     (qTokens, qTags) = delexicalizeQuestion(id, wqa.question).unzip
//   //     alignments = getQuestionSentenceAlignments(sentenceTokens.toVector.map(_.token), qTokens.toVector)
//   //     sQuestionIndices = alignments.map(_._2)
//   //     qSentenceIndices = alignments.map(_._1)
//   //     answer <- wqa.answer :: answers
//   //   } yield {
//   //     val sTags = sentenceTokens.zipWithIndex.map { case(POSTaggedToken(_, pos), i) =>
//   //       val placementTag = if(sQuestionIndices(i)) "Q" else if(answer(i)) "A" else "O"
//   //       s"$placementTag-$pos"
//   //     }.mkString(" ")
//   //     val line = s"${sentenceTokens.map(_.token).mkString(" ")} ||| $sTags ||| ${qTokens.mkString(" ")} ||| ${qTags.mkString(" ")}\n"
//   //     sb.append(line)
//   //   }

//   //   saveDataFile(experimentName, filename, sb.toString)
//   // }
//   lazy val questionDuplicationCounts = Scorer[Int, Int](data.all.groupBy(_.question).map(_._2.size))

//   lazy val (anyWhQuestions, noWhQuestions) = data.all.partition(q => q.questionTokens.map(_.toLowerCase).exists(questionWords.contains))
//   lazy val (beginWhQuestions, nonBeginWhQuestions) =
//     anyWhQuestions.partition(q => questionWords.contains(q.questionTokens.head.toLowerCase))

//   lazy val nonBeginWhWordBreakdown = Scorer[String, Int](
//     anyWhQuestions.map(
//       _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
//     ).iterator
//   )
//   lazy val nonBeginWhWordBreakdownReport = nonBeginWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
//     case (word, count) => s"$word: ${pctString(count, nonBeginWhQuestions.size)}"
//   }.mkString("\n")

//   lazy val beginWhWordBreakdown = Scorer[String, Int](
//     beginWhQuestions.map(
//       _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
//     ).iterator
//   )
//   lazy val beginWhWordBreakdownReport = beginWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
//     case (word, count) => s"$word: ${pctString(count, beginWhQuestions.size)}"
//   }.mkString("\n")

//   lazy val anyWhWordBreakdown = Scorer[String, Int](
//     anyWhQuestions.map(
//       _.questionTokens.map(_.lowerCase).find(lowerQWords.contains).map(_.toString).getOrElse("<N/A> (shouldn't happen)")
//     ).iterator
//   )
//   lazy val anyWhWordBreakdownReport = anyWhWordBreakdown.iterator.toVector.sortBy(-_._2).map {
//     case (word, count) => s"$word: ${pctString(count, anyWhQuestions.size)}"
//   }.mkString("\n")

//   lazy val report = s"""
// Number of valid questions: ${data.all.size}
// Question duplications:
// ${histogramString(questionDuplicationCounts)}
// Number of questions beginning with a question word: ${pctString(beginWhQuestions.size, data.all.size)}
// Number of questions with question word not at beginning: ${pctString(nonBeginWhQuestions.size, data.all.size)}
// Number of questions with no question word: ${pctString(noWhQuestions.size, data.all.size)}
// First question word breakdown:
// $anyWhWordBreakdownReport

// Non-beginning word breakdown:
// $nonBeginWhWordBreakdownReport

// Beginning wh-words:
// $beginWhWordBreakdownReport
// """.trim
// }
