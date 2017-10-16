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
// import nlpdata.datasets.wiktionary.Inflections

package object analysis {

  object AbstractSlot {
    implicit val abstractSlotShow: Show[AbstractSlot] = Show.show(_ => "_")
  }
  type AbstractSlot = AbstractSlot.type

  import Datasets._
  import Reinflection._
  import TemplateToken._

  // QuestionTemplate is global across all sentences.

  case class ContiguousSpan(begin: Int, end: Int) {
    def indices = (begin to end).toSet
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
}
