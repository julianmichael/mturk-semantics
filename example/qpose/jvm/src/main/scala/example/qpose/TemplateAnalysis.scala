package example.qpose

import example.emnlp2017.SentenceId
import example.emnlp2017.isStopword
import example.emnlp2017.saveOutputFile

import cats.Show
import cats.Order
import cats.implicits._

import qamr.QAData
import qamr.SourcedQA
import qamr.QAPairId

import turksem.gapfill._
import turksem.util._

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
import nlpdata.structure.Word
import nlpdata.util.Text
import nlpdata.util.PosTags
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._

// NOTE might generalize slot type in the future, but probably not
class TemplateAnalysis(
  label: String,
  data: QAData[SentenceId]) {

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

  val templatingPhases = new TemplatingPhases[SentenceId]
  import templatingPhases._

  // NOTE consider removing adjunct mod filtering, if I want...probably not
  // NOTE fold in genitive clitics too? nah
  val fullPos = posPhase andThen filterAdjunctModPhase
  def postprocess(phase: TemplatingPhase[SentenceId]) =
    phase andThen /* collapseContigProperNounsPhase andThen */ deleteRedundantDeterminersPhase

  // TODO add simple templates for "how adj" and "how adv" and maybe some others to run before adjunct mod filtering

  val badTemplates = Set(
    QuestionTemplate[TriggerSlot](
      List(TemplateString("what".lowerCase), TemplateSlot(TriggerSlot("NOUN", true)))
    ),
    QuestionTemplate[TriggerSlot](
      List(TemplateString("what".lowerCase), TemplateSlot(TriggerSlot("NOUN-pl", true)))
    ),
    QuestionTemplate[TriggerSlot](
      List(TemplateString("what".lowerCase), TemplateSlot(TriggerSlot("NOUN-prop", true)))
    ),
    QuestionTemplate[TriggerSlot](
      List(TemplateString("what".lowerCase), TemplateSlot(TriggerSlot("NOUN-prop-pl", true)))
    )
  )

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
         foldDeterminersPhase, dropNoSlotTemplatePhase, dropPOSPhase,
         filterInfrequentTemplatesPhase(5), filterBadTemplatesPhase(badTemplates.contains))
  ).flatten

  lazy val templatingPipeline = defaultPipeline

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

  object PosMatchers {
    object noun {
      def unapply(w: Word): Option[Int] =
        if(PosTags.nounPosTags.contains(w.pos) || w.pos == "PRP") Some(w.index) else None
    }
    object verb {
      def unapply(w: Word): Option[Int] =
        if(PosTags.verbPosTags.contains(w.pos)) Some(w.index) else None
    }

    object adj { def unapply(w: Word): Boolean = PosTags.adjectivePosTags.contains(w.pos) }
    object det { def unapply(w: Word): Boolean = w.pos == "DT" }
    object gen { def unapply(w: Word): Boolean = w.pos == "PRP$" }
    object pos { def unapply(w: Word): Boolean = w.pos == "POS" }
    object num { def unapply(w: Word): Boolean = w.pos == "CD" }
    object prep { def unapply(w: Word): Boolean = w.pos == "IN" }

    object MatchBareNoun {
      def unapply(ws: List[Word]): Option[Int] = ws match {
        case noun(i) :: Nil => Some(i)
        case adj :: noun(i) :: Nil => Some(i)
        case noun(_) :: pos :: noun(i) :: Nil => Some(i)
        case noun(_) :: pos :: adj :: noun(i) :: Nil => Some(i)
        case num :: noun(i) :: Nil => Some(i)
        case _ => None
      }
    }

    object MatchAnyNoun {
      def unapply(ws: List[Word]): Option[Int] = ws match {
        case MatchBareNoun(i) => Some(i)
        case det :: MatchBareNoun(i) => Some(i)
        case noun(_) :: pos :: MatchBareNoun(i) => Some(i)
        case gen :: MatchBareNoun(i) => Some(i)
        case _ => None
      }
    }
  }

  val alignmentsWithLabeledAnswers = alignmentsById.flatMap { case (id, qta) =>
    val sid = id.sentenceId
    val posTaggedSentence = PosTagger.posTag(sid.tokens)
    import PosMatchers._
    val matchies = qta.answers.map(a => a.toList.sorted.map(posTaggedSentence)).map {
      case MatchAnyNoun(i) => Some("NOUN" -> i)
      case verb(i) :: _ => Some("VERB" -> i)
      case _ => None
    }
    if(matchies.flatten.isEmpty) None
    else {
      val matchiesSortedByCount = matchies.flatten
        .groupBy(identity)
        .toVector
        .sortBy(-_._2.size)
      val mostCommonMatchies = matchiesSortedByCount
        .takeWhile(_._2 == matchiesSortedByCount.head._2)
        .map(_._1)
      val (matchLabel, answerIndex) = {
        if(mostCommonMatchies.size == 1) mostCommonMatchies.head
        else getTriggerSpan(qta).fold(mostCommonMatchies.head) { triggerSpan =>
          mostCommonMatchies.sortBy(p => math.abs(p._2 - triggerSpan.begin)).head
        }
      }
      Some(id -> (qta, matchLabel, answerIndex))
    }
  }

  // TODO LIST:
  // - redo paraphrasing in the stricter way with alignment
  // - add mirroring in addition to paraphrasing: each set corresponds to a "role", we can find it with the right trigger
  // - make both paraphrasing and mirroring DIRECTIONAL (i.e. look like entailment)? regardless,
  //   - mirroring should either not be transitive or not be symmetric, as a starting point.
  // - for cases of mirroring, identify which side is the more useful trigger
  // - change templating logic to replace all parts of speech as we see fit... maybe

  // TODO redo paraphrasing in this much stricter way
  // case class ParaphrasedTemplateAlignment(
  //   template1: QuestionTemplateAlignment[SentenceId, TriggerSlot],
  //   template2: QuestionTemplateAlignment[SentenceId, TriggerSlot],
  //   permutation: List[Int] // only permutes question alignments
  // )
  // object ParaphrasedTemplateAlignment {
  //   def getPossibleAlignments(
  //     x: QuestionTemplateAlignment[SentenceId, TriggerSlot],
  //     y: QuestionTemplateAlignment[SentenceId, TriggerSlot]
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
    x: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    y: QuestionTemplateAlignment[SentenceId, TriggerSlot]
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
    x: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    y: QuestionTemplateAlignment[SentenceId, TriggerSlot]
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
      QuestionTemplateAlignment[SentenceId, TriggerSlot],
      QuestionTemplateAlignment[SentenceId, TriggerSlot]
    ) => Option[Double]
  ) = alignmentsBySentenceId.iterator.flatMap { case (id, qtasForSentence) =>
    qtasForSentence
      .map(_._2)
      .groupBy(qta => getTriggerWordStem(qta))
      .flatMap { case (triggerStem, qtasForWord) =>
        def getAllPairwiseScores(
          qtas: List[QuestionTemplateAlignment[SentenceId, TriggerSlot]]
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
    case ((template1, template2), similarities) if template1 != template2 =>
      AggregateSimilarity(template1, template2, similarities.map(_._3).toList.sum, similarities.size)
  }.toVector.sorted

  def templateClusters(
    chosenSimilarities: Vector[AggregateSimilarity] = similarities(computeSetwiseAnswerSimilarity),
    numInstancesThreshold: Int = 10,
    accuracyThreshold: Double = 0.98
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

  // case class AbstractTemplateAlignment(
  //   source: QuestionTemplate[TriggerSlot],
  //   target: QuestionTemplate[TriggerSlot],
  //   answerAlignmentOpt: Option[TemplateAlignmentIndex],
  //   questionAlignments: List[TemplateAlignmentIndex] // length = number of slots in target, each is index into source QA
  // )
  // object AbstractTemplateAlignment {
  // }

  // TODO make use of clusters to get better alignments?
  // here be a vector of all of the alignments
  lazy val directedTemplateAlignments = {
    alignmentsBySentenceId.iterator.flatMap { case (id, qtasForSentence) =>
      val sentenceTokens = id.tokens
      qtasForSentence.flatMap { case (_, sourceQTA) =>
        val singleWordAnswerIndices = sourceQTA.sourcedQA.answers.filter(_.size == 1).map(_.head).distinct
        val singleWordQuestionAlignments = sourceQTA.alignments.map(_.filter(_.indices.size == 1).map(_.begin))
        val triggerSentenceIndices = sourceQTA.alignments(getTriggerSlotIndex(sourceQTA.template)).map(_.begin).toSet
        // excludes questions with noun placeholders
        val allSourceAlignments = (singleWordAnswerIndices :: singleWordQuestionAlignments).sequence
        // TODO allow source to have "something"s and target to have external NOUN-dets (which may also be "something")?
        for {
          (_, targetQTA) <- qtasForSentence
          if targetQTA != sourceQTA // don't match with self
          if targetQTA.alignments.size == sourceQTA.alignments.size + 1 || targetQTA.alignments.size == sourceQTA.alignments.size
          results = {
            val needToAlignTargetAnswer = targetQTA.alignments.size == sourceQTA.alignments.size
            // excludes questions with noun placeholders
            val targetQuestionIndexChoices = targetQTA.alignments
              .map(_.filter(_.indices.size == 1).map(_.begin))
              .sequence.toSet
            for {
              sourceAnswerIndex :: sourceQuestionAlignedIndices <- allSourceAlignments
              targetQuestionAlignedIndices <- targetQuestionIndexChoices
              (crossTemplateAlignment: List[TemplateAlignmentIndex]) <- targetQuestionAlignedIndices.map(i =>
                (if(i == sourceAnswerIndex) List(AnswerIndex) else Nil) ++
                  sourceQuestionAlignedIndices.collectIndices{ case `i` => true }.map(QuestionSlotIndex(_))
              ).sequence.toList.distinct
              if targetQTA.template.toList.zip(crossTemplateAlignment).forall {
                case (TriggerSlot(label, _), QuestionSlotIndex(i)) =>
                  label.startsWith(
                    sourceQTA.template.toList(i).label.takeWhile(_ != '-')
                  )
                case _ => true
              }
              answerAlignmentOpt <- {
                if(needToAlignTargetAnswer) {
                  val targetAnswerIndices = targetQTA.sourcedQA.answers.filter(_.size == 1).map(_.head).distinct
                  targetAnswerIndices.toList.map(i =>
                    (if(i == sourceAnswerIndex) List(AnswerIndex) else Nil) ++
                      sourceQuestionAlignedIndices.collectIndices{ case `i` => true }.map(QuestionSlotIndex(_))
                  ).flatMap(alignedIndices =>
                    if(alignedIndices.nonEmpty) alignedIndices.map(Option(_)) else List(None)
                  ).distinct
                } else List(None)
              }
              resultDTA <- {
                if(needToAlignTargetAnswer) {
                  Some(ParaphraseAlignment(sourceQTA, targetQTA, crossTemplateAlignment, answerAlignmentOpt))
                } else if(crossTemplateAlignment.contains(AnswerIndex)) {
                  Some(DirectToQuestionAlignment(sourceQTA, targetQTA, crossTemplateAlignment))
                } else None
              }
            } yield resultDTA
          }
          result <- results.distinct
        } yield result
      }
    }.toVector.groupBy(_.map(_.template)).toVector.sortBy(-_._2.size)
  }

  lazy val directToQuestionAlignments = directedTemplateAlignments.collect {
    case (dtqa: DirectToQuestionAlignment[QuestionTemplate[TriggerSlot]], xs) => dtqa -> xs
  }

  lazy val questionExpansionProbabilities = directToQuestionAlignments
    .groupBy(_._1.source)
    .map { case (source, dtqas) =>
      val targetFreqs = dtqas.map { case (dtqa, alignmentDTQAs) => dtqa -> alignmentDTQAs.size }.toMap
      source -> NextQuestionProbabilities(source, templateCounts(source), targetFreqs)
  }

  lazy val paraphraseAlignments = directedTemplateAlignments.collect {
    case (pa: ParaphraseAlignment[QuestionTemplate[TriggerSlot]], xs) => pa -> xs
  }

  case class ScoredParaphraseAlignment(
    source: QuestionTemplate[TriggerSlot],
    target: QuestionTemplate[TriggerSlot],
    questionAlignments: List[TemplateAlignmentIndex],
    answerDistribution: Map[Option[TemplateAlignmentIndex], Double],
    frequency: Int
  )
  object ScoredParaphraseAlignment {
    implicit val scoredParaphraseAlignmentShow: Show[ScoredParaphraseAlignment] = new Show[ScoredParaphraseAlignment] {
      override def show(spa: ScoredParaphraseAlignment): String = {
        val sourceTemplateStr =  spa.source.mapWithIndex((sl: TriggerSlot, i: Int) => s"${sl.label}-$i").show
        val targetTemplateStr = spa.target.mapWithZippedList(
          spa.questionAlignments, (sl: TriggerSlot, iOpt: Option[TemplateAlignmentIndex]) => s"${sl.label}-${iOpt.get.show}"
        ).show
        val targetAnswerStr = spa.answerDistribution.toVector.sortBy(-_._2).map {
          case (taiOpt, p) => f"${taiOpt.fold("<new>")(_.show)}%s ($p%.2f)"
        }
        f"${spa.frequency}%6d $sourceTemplateStr%-50s --> $targetTemplateStr%-50s $targetAnswerStr"
      }
    }
  }

  lazy val scoredParaphraseAlignments = paraphraseAlignments
    .groupBy { case (epa, xs) => (epa.source, epa.target, epa.questionAlignments) }
    .toVector
    .map {
    case ((source, target, questionAlignments), epas) =>
      val total = epas.map(_._2.size).sum
      val answerDist = epas.groupBy(_._1.answerAlignmentOpt).map {
        case (answerAlignmentOpt, epas) => answerAlignmentOpt -> epas.map(_._2.size).sum.toDouble / total
      }
      ScoredParaphraseAlignment(source, target, questionAlignments, answerDist, total)
  }

  val allTriggerLabels = List(
    "NOUN", "NOUN-pl", "NOUN-prop", "NOUN-pl", "NOUN-prop-pl",
    "VERB", "VERB-pss",
    "ADJ", "ADJ-cmp", "ADJ-sup"
  )

  def getCompatibleLabelsForLabel(label: String) = label match {
    case "NOUN-pl" => List("NOUN-pl", "NOUN", "NOUN-det")
    case "NOUN-prop" => List("NOUN-prop", "NOUN-det")
    case "NOUN-prop-pl" => List("NOUN-prop-pl", "NOUN-pl", "NOUN-det")
    case "VERB" => List("VERB", "VERB-pss")
    case x => List(x)
  }


  lazy val starterTemplates = templatesByFrequency
    .takeWhile(_._2 >= 10)
    .filter(_._1.size == 1)

  lazy val starterTemplateCountsByFineGrainedLabel = starterTemplates
    .groupBy(t => t._1.headOption.get.label)
    .withDefaultValue(Nil)

  lazy val starterTemplateCountsByCoarseGrainedLabel: Map[String, List[(QuestionTemplate[TriggerSlot], Int)]] = allTriggerLabels.map { label =>
    label -> getCompatibleLabelsForLabel(label).flatMap(starterTemplateCountsByFineGrainedLabel)
  }.toMap

  lazy val starterTemplateProbsByCoarseGrainedLabel = starterTemplateCountsByCoarseGrainedLabel.map {
    case (label, xs) =>
      val totalCount = xs.map(_._2).sum
      label -> xs.map { case (t, count) => t -> count.toDouble / totalCount }
  }

  class QuestionSuggester {

    def expandedQuestionLogProbabilities(state: QuestioningState) = {
      // TODO list all candidate target questions,
      // then get their probabilities according to each existing question (which shares enough arguments with it),
      val allTargetQuestions = for {
        (group, groupIndex) <- state.triggerGroups.zipWithIndex
        (JudgedQuestion(InstantiatedQuestion(template, sourceArgs), Answer(_)), questionIndex) <- group.qas.zipWithIndex
        nextQuestionProbabilities <- questionExpansionProbabilities.get(template).toList
        transformation <- nextQuestionProbabilities.targetFrequencies.keys
      } yield {
        // val prob = nextQuestionProbabilities.getTargetLogProbability(transformation.target)
        val targetArgs = transformation.questionAlignments.map {
          case AnswerIndex => PreviousAnswerSpan(groupIndex, questionIndex)
          case QuestionSlotIndex(i) => sourceArgs(i)
        }
        InstantiatedQuestion(transformation.target, targetArgs)
      }
      val probabilitiesOfEachQA = allTargetQuestions.map {
        case iq @ InstantiatedQuestion(target, targetArgs) =>
          val logProbs = for {
            (group, groupIndex) <- state.triggerGroups.zipWithIndex
            (JudgedQuestion(InstantiatedQuestion(source, sourceArgs), Answer(_)), questionIndex) <- group.qas.zipWithIndex
            // assess if source question is relevant to target
            if targetArgs.size == (sourceArgs.size + 1)
            nextQuestionProbabilities <- questionExpansionProbabilities.get(source)
            alignment <- targetArgs.map {
              case PreviousAnswerSpan(`groupIndex`, `questionIndex`) => Some(AnswerIndex)
              case otherArg => sourceArgs.findIndex(_ == otherArg).map(QuestionSlotIndex(_))
            }.sequence
          } yield nextQuestionProbabilities.getTargetLogProbability(
            DirectToQuestionAlignment(source, target, alignment)
          )
          // going to be nonempty (assuming no bugs) because each was generated by at least one question
          iq -> logProbs.sum
      }.groupBy { case (iq, _) =>
          state.renderQuestion(iq)
      }.map { case (_, iqsWithProbs) => iqsWithProbs.head
      }.toVector.sortBy(-_._2)
      probabilitiesOfEachQA
    }

    def nextQuestionLogProbabilitiesForTrigger(state: QuestioningState, triggerGroupIndex: Int) = {
      val triggerGroup = state.triggerGroups(triggerGroupIndex)
      val allQuestionStrings = state.triggerGroups.flatMap(_.qas).map(qa => state.renderQuestion(qa.question)).toSet
      val initLogProbs = starterTemplateProbsByCoarseGrainedLabel(triggerGroup.triggerLabel).map {
        case (template, prob) => InstantiatedQuestion(template, List(SentenceWord(triggerGroup.trigger.index))) -> math.log(prob)
      }
      val expansionLogProbs = expandedQuestionLogProbabilities(state)
      val expansionInterpolationFactor = state.triggerGroups.size.toDouble / (state.triggerGroups.size + 1)
      val initInterpolationFactor = 1.0 - expansionInterpolationFactor
      val fullRescaledProbsIter = initLogProbs.iterator.map {
        case (iq, logProb) => iq -> (logProb + math.log(initInterpolationFactor))
      } ++ expansionLogProbs.iterator.map {
        case (iq, logProb) => iq -> (logProb + math.log(expansionInterpolationFactor))
      }
      // TODO scale by probability that the question is not covered yet
      fullRescaledProbsIter.filter {
        case (iq @ InstantiatedQuestion(_, args), _) =>
          args.contains(SentenceWord(triggerGroup.trigger.index)) &&
            !allQuestionStrings.contains(state.renderQuestion(iq))
      }.toVector.sortBy(-_._2)
    }

    // def nextQuestionProbabilitiesForTrigger(
    //   state: QuestioningState,
    // )

    def groupQAsL(groupIndex: Int) = QuestioningState.triggerGroups
      .composeOptional(Optics.index(groupIndex))
      .composeLens(TriggerGroup.qas)

    def runConsoleQuestionSuggestion(sid: SentenceId): Unit = {
      var qState = QuestioningState.initFromSentence(getInflectionalSentence(sid))
      var cmd = "init"
      var curGroup = 0
      val scanner = new java.util.Scanner(java.lang.System.in)
      while(cmd != "") {
        println; println
        println(Text.render(sid))
        qState.triggerGroups.zipWithIndex.foreach { case (TriggerGroup(trigger, triggerLabel, qas), groupIndex) =>
          println(s"$groupIndex: ${trigger.token} (${trigger.index}, $triggerLabel):")
          qas.zipWithIndex.foreach { case (JudgedQuestion(iq, judgment), qaIndex) =>
            val judgmentStr = judgment match {
              case BadQuestion => "N/A"
              case Answer(span) => Text.renderSpan(sid, span)
            }
            println("\t" + qaIndex + ": " + qState.renderQuestion(iq) + " --> " + judgmentStr)
          }
        }
        val nextQuestionsWithLogProb = nextQuestionLogProbabilitiesForTrigger(qState, curGroup)
        println("Next questions:")
        nextQuestionsWithLogProb.take(10).foreach {
          case (iq, logProb) =>
            println(f"${qState.renderQuestion(iq)}%-50s $logProb%7.5f")
        }
        println
        val iqOpt = nextQuestionsWithLogProb.headOption.map(_._1)
        iqOpt match {
          case None => println("No more questions. ???")
          case Some(iq) => println(qState.renderQuestion(iq))
        }
        cmd = scanner.nextLine
        val triggerRegex = """t: ([0-9]+)""".r
        val cmdTokens = Tokenizer.tokenize(cmd).toList
        val recoveredSpanOpt = if(cmdTokens.size == 0) None else sid.tokens.sliding(cmdTokens.size).toList
          .findIndex(window => window.toList == cmdTokens)
          .map(start => (start until (start + cmdTokens.size)).toSet)
        (cmd, iqOpt, recoveredSpanOpt) match {
          case (triggerRegex(IntMatch(i)), _, _) if i >= 0 && i < qState.triggerGroups.size =>
            curGroup = i
          case ("N/A", Some(iq), _) =>
            qState = groupQAsL(curGroup).modify(_ :+ JudgedQuestion(iq, BadQuestion))(qState)
          case (answer, Some(iq), Some(recoveredSpan)) if answer != "" =>
            qState = groupQAsL(curGroup).modify(_ :+ JudgedQuestion(iq, Answer(recoveredSpan)))(qState)
          case _ => ()
        }
      }
    }
  }

  // TODO only do this paraphrasing stuff if absolutely necessary
  // lazy val properParaphraseClusterMapping = {
  //   case class ParaphrasingRealignments(
  //     canonicalTemplate: QuestionTemplate[TriggerSlot],
  //     canonicalTemplateFreq: Int,
  //     paraphraseMappings: Map[QuestionTemplate[TriggerSlot], List[QuestionSlotIndex]])
  //   // TODO rephrase stuff in terms of these explicit realignments so the semigroup combines and rearranges the alignments appropriately
  //   object TemplateFreqWrapper {
  //     implicit val templateFreqWrapperOrder = Order.by[TemplateFreqWrapper, Int](_.frequency)
  //     implicit val templateFreqWrapperCommutativeSemigroup = new CommutativeSemigroup[TemplateFreqWrapper] = {
  //       def combine(x: TemplateFreqWrapper, y: TemplateFreqWrapper) =
  //         if(x.frequency >= y.frequency) x else y
  //     }
  //   }
  //   val clusters = MutableUnionFindTagged.empty[QuestionTemplate[TriggerSlot], TemplateFreqWrapper]
  //   def unifiableAlignmentIterator = scoredParaphraseAlignments.iterator
  //     .filter(spa => spa.score >= 0.8 && spa.frequency > 1)
  //   val eligibleTemplates = unifiableAlignmentIterator
  //     .flatMap(spa => List(spa.source, spa.target))
  //     .toSet
  //   eligibleTemplates.foreach { t =>
  //     clusters.add(t, TemplateFreqWrapper(t, templateCounts(t)))
  //   }
  //   unifiableAlignmentIterator.foreach { spa =>
  //     clusters.union(spa.source, spa.target)
  //   }
  //   def templateToCanonicalParaphraseIterator = for {
  //     t <- clusters.iterator
  //     rep <- clusters.findTag(t)
  //   } yield t -> rep.template
  //   templateToCanonicalParaphraseIterator.toMap
  // }

  // TODO do if we reaaaally need to
  // lazy val templateSetsByWord = {
  //   alignmentsBySentenceId.iterator.flatMap { case (sid, qtasById) =>
  //     val tokens = sid.tokens
  //     qtasById.flatMap { case (_, qta) =>
  //       qta.template.toList.zipWithIndex.zip(qta.alignments)
  //     }
  //   }
  // }

  def printAnalysis = {
    println(s"Intermediate results:")
    namedTemplatingResults.foreach { case (name, result) =>
      println
      println(s"Phase: ${name.capitalize}")
      println(f"Coverage: ${result.proportionQAsCovered}%.2f")
      println(f"Turnover: ${result.proportionQAsWithNewTemplate}%.2f")

      def printFrequentExampleAlignments(
        alignments: Map[QAPairId[SentenceId], QuestionTemplateAlignment[SentenceId, TriggerSlot]]
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

  def getTriggerSpan(qta: QuestionTemplateAlignment[SentenceId, TriggerSlot]): Option[ContiguousSpan] = {
    qta.alignments(getTriggerSlotIndex(qta.template)).headOption
  }

  def getTriggerWordStem(qta: QuestionTemplateAlignment[SentenceId, TriggerSlot]): String = {
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
          .getOrElse(Map.empty[QAPairId[SentenceId], QuestionTemplateAlignment[SentenceId, TriggerSlot]])
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
