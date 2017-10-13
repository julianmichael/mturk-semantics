package example.emnlp2017.analysis

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
import nlpdata.datasets.wiktionary.Inflections

import monocle.macros.Lenses

@Lenses case class TriggerSlot(
  label: String,
  isTrigger: Boolean = false) {
  def withLabel(newLabel: String) = TriggerSlot(newLabel, isTrigger)
  def withIsTrigger(newIsTrigger: Boolean) = TriggerSlot(label, newIsTrigger)
}
object TriggerSlot {
  implicit val triggerSlotShow = new Show[TriggerSlot] {
    override def show(ts: TriggerSlot) = if(ts.isTrigger) ts.label + "*" else ts.label
  }
}

// NOTE might generalize slot types in the future, but probably not
@Lenses case class TemplatingResult(
  previousAlignments: Map[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]],
  resultAlignments: Map[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]],
  unalignedQuestions: Map[QAPairId[SentenceId], SourcedQA[SentenceId]]
) {

  val previousTemplates = previousAlignments.map(_._2.template).toSet
  val resultTemplates = resultAlignments.map(_._2.template).toSet

  val newQuestionAlignments = resultAlignments.filterNot(pair => previousAlignments.contains(pair._1))
  val lostQuestionAlignments = previousAlignments.filterNot(pair => resultAlignments.contains(pair._1))

  val newTemplateAlignments = resultAlignments
    .filterNot { case (id, qta) => previousTemplates.contains(qta.template) }

  val lostTemplateAlignments = previousAlignments
    .filterNot { case (id, qta) => resultTemplates.contains(qta.template) }

  val totalNumQuestions = resultAlignments.size + unalignedQuestions.size

  def proportionQAsCovered = resultAlignments.size.toDouble / totalNumQuestions

  def proportionQAsWithNewTemplate = newTemplateAlignments.size.toDouble / totalNumQuestions

}

object TemplatingResult {

  def initFromQuestions(sqasById: Map[QAPairId[SentenceId], SourcedQA[SentenceId]]) = TemplatingResult(
    previousAlignments = Map.empty[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]],
    resultAlignments = Map.empty[QAPairId[SentenceId], QuestionTemplateAlignment[TriggerSlot]],
    unalignedQuestions = sqasById)

}

case class TemplatingPhase(
  name: String,
  run: TemplatingResult => TemplatingResult
) {
  // to give us options in how we see what is gained and lost in the pipeline at each step
  def andThen(that: TemplatingPhase) = TemplatingPhase(
    this.name + " and then " + that.name, { prev =>
      val newResult = that.run(this.run(prev))
      newResult.copy(previousAlignments = prev.resultAlignments)
    }
  )
}

// TODO: collapse contiguous proper nouns, maybe match up placeholders in noun templates

object TemplatingPhase {

  def phaseFromOptionalTemplating(
    name: String,
    func: SourcedQA[SentenceId] => Option[QuestionTemplateAlignment[TriggerSlot]]
  ): TemplatingPhase = TemplatingPhase(
    name, { case TemplatingResult(_, prevAlignments, questionsToGo) =>
      val newAlignments = questionsToGo.flatMap {
        case (id, sqa) => func(sqa).map(id -> _)
      }
      TemplatingResult(
        prevAlignments,
        newAlignments ++ prevAlignments,
        questionsToGo -- newAlignments.keys)
    }
  )

  def phaseFromAlignmentProcessor(
    name: String,
    process: QuestionTemplateAlignment[TriggerSlot] => Option[QuestionTemplateAlignment[TriggerSlot]]
  ) = TemplatingPhase(
    name, result => TemplatingResult(
      result.resultAlignments,
      result.resultAlignments.map { case (id, qta) => id -> process(qta).getOrElse(qta) },
      result.unalignedQuestions)
  )

  def phaseFromTemplateProcessor(
    name: String,
    process: QuestionTemplate[TriggerSlot] => Option[QuestionTemplate[TriggerSlot]]
  ) = phaseFromAlignmentProcessor(name, qta => process(qta.template).map(newTemplate => qta.copy(template = newTemplate)))

  def phaseFromFilter(
    name: String,
    predicate: QuestionTemplateAlignment[TriggerSlot] => Boolean
  ) = TemplatingPhase(
    name, prev => {
      val questionsToDrop = prev.resultAlignments.collect {
        case (id, qta) if !predicate(qta) => id -> qta.sourcedQA
      }
      TemplatingResult(
        previousAlignments = prev.resultAlignments,
        resultAlignments = (prev.resultAlignments -- questionsToDrop.keys),
        unalignedQuestions = prev.unalignedQuestions ++ questionsToDrop)
    }
  )

  val oneWordOnlyPhase = phaseFromFilter(
    "retain one-word templates only", _.template.size == 1
  )

  val dropPOSPhase = phaseFromFilter(
    "drop POS templates", qta => !qta.template.exists(ts => PosTags.allPosTags.contains(ts.label))
  )

  val preStemAuxes = Set(
    "do", "did", "does", "to"
  ).map(_.lowerCase) ++ Inflections.willVerbs ++ Inflections.modalVerbs ++ Inflections.wouldVerbs

  def chooseStemOrNon3rdSgPresent(qTokens: Vector[LowerCaseString], verbIndex: Int): String =
    if(qTokens.take(verbIndex).exists(preStemAuxes.contains)) "VB" else "VBP"

  def templatizeByPos(
    sqa: SourcedQA[SentenceId]
  ): QuestionTemplateAlignment[TriggerSlot] = {
    val sentenceTokens = sqa.id.sentenceId.tokens
    val posTaggedSentenceTokens = posTag(sentenceTokens)
    val qTokens = {
      val toks = tokenize(sqa.question)
      if(toks.last == "?") toks
      else toks ++ List("?")
    }
    val alignments = getReinflectedQuestionSentenceAlignments(sentenceTokens, qTokens)
    val alignedQIndices = alignments.keySet.size
    val posPairsAndAlignments = for {
      (alignedQIndex, possiblyReinflectedSentenceAlignments) <- alignments.toList
      alignedSentenceIndicesWithSameToken = possiblyReinflectedSentenceAlignments.collect {
        case InflectedAlignment(i, None) => i
      }.sorted
      (pos, finalAlignments) = alignedSentenceIndicesWithSameToken.headOption match { // choose first arbitrarily
        case Some(sIndex) =>
          val thisPos = posTaggedSentenceTokens(sIndex).pos
          val newPos = if(thisPos == "VB" || thisPos == "VBP") {
            chooseStemOrNon3rdSgPresent(qTokens.map(_.lowerCase), alignedQIndex)
          } else thisPos
          (newPos, alignedSentenceIndicesWithSameToken)
        case None => // must have changed the form of the word
          val reinflectedSentenceAlignments = possiblyReinflectedSentenceAlignments.collect {
            case InflectedAlignment(i, Some(form)) => (i, form)
          }
          val (chosenSIndex, chosenReinflection) = reinflectedSentenceAlignments.head
          val alignedPOS = posTaggedSentenceTokens(chosenSIndex).pos // choose first arbitrarily. this always exists
          val chosenPOS = if(PosTags.verbPosTags.contains(alignedPOS)) chosenReinflection match {
            case 0 => chooseStemOrNon3rdSgPresent(qTokens.map(_.lowerCase), alignedQIndex)
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
      alignedSentenceSpans = finalAlignments.map(i => ContiguousSpan(i, i))
    } yield (alignedQIndex -> pos, alignedSentenceSpans)
    val (posPairs, finalAlignments) = posPairsAndAlignments.unzip
    val templateTokens = posPairs.foldLeft(qTokens.toList.map(tok => TemplateString(tok.lowerCase): TemplateToken[TriggerSlot])) {
      case (tokens, (alignedQIndex, pos)) => tokens.updated(alignedQIndex, TemplateSlot(TriggerSlot(pos)))
    }
    QuestionTemplateAlignment(sqa, QuestionTemplate(templateTokens), finalAlignments)
  }

  lazy val posPhase = phaseFromOptionalTemplating("POS", (templatizeByPos _) andThen Option.apply)

  val collapseContigProperNounsPhase = TemplatingPhase(
    "collapse contiguous proper nouns", {
      case TemplatingResult(_, prevAlignments, unalignedQuestions) => TemplatingResult(
        prevAlignments,
        prevAlignments.map { case (id, QuestionTemplateAlignment(sourcedQA, template, alignments)) =>
          val (newTemplateTokens, newAlignments, _) = template.templateTokens.foldRight(
            (List.empty[TemplateToken[TriggerSlot]], List.empty[List[ContiguousSpan]], alignments.reverse)) {
            case (
              TemplateSlot(TriggerSlot("NNP", isNewTrigger)),
              (TemplateSlot(TriggerSlot("PNOUN", wasOldTrigger)) :: tokens, prevAlignment :: finishedAlignments, curAlignment :: remainingAlignments)) =>
              val newPossibleAlignment = for {
                prev <- prevAlignment
                cur <- curAlignment
                if cur.end == prev.begin - 1
              } yield ContiguousSpan(cur.begin, prev.end)
              if(newPossibleAlignment.isEmpty) {
                (TemplateSlot(TriggerSlot("PNOUN", isNewTrigger)) :: TemplateSlot(TriggerSlot("PNOUN", wasOldTrigger)) :: tokens, curAlignment :: prevAlignment :: finishedAlignments, remainingAlignments)
              } else {
                (TemplateSlot(TriggerSlot("PNOUN", isNewTrigger || wasOldTrigger)) :: tokens, newPossibleAlignment :: finishedAlignments, remainingAlignments)
              }
            case (
              TemplateSlot(TriggerSlot("NNP", isNewTrigger)),
              (tokens, finishedAlignments, curAlignment :: remainingAlignments)
            ) => (TemplateSlot(TriggerSlot("PNOUN", isNewTrigger)) :: tokens, curAlignment :: finishedAlignments, remainingAlignments)
            case (t @ TemplateString(_), (tokens, alignments, remainingAlignments)) =>
              (t :: tokens, alignments, remainingAlignments)
            case (t @ TemplateSlot(_), (tokens, alignments, curAlignment :: remainingAlignments)) =>
              (t :: tokens, curAlignment :: alignments, remainingAlignments)
          } // TODO is this above exhaustive
          val newTemplate = QuestionTemplate(newTemplateTokens).map(
            TriggerSlot.label.modify {
              case "PNOUN" => "NNP"
              case x => x
            }
          )
          id -> QuestionTemplateAlignment(sourcedQA, newTemplate, newAlignments)
        },
        unalignedQuestions
      )
    }
  )


  val nounPlaceholderWords = Set(
    "someone", "something",
    "I", "we", "you", "he", "she", "it", "they",
    "this", "that", "those", "these",
    "him", "her", "me", "us", "them",
    "myself", "ourselves", "yourself", "himself", "herself", "itself", "themselves"
  ).map(_.lowerCase)


  val determiners = Set("the", "a", "an", "this", "these", "those").map(_.lowerCase)

  val determinedNounPosTags = Set("NNP", "PRP", "DT")
  val pluralNounPosTags = Set("NNS", "NNPS")
  val undeterminedNounPosTags = Set("NN")
  val allNounPosTags = determinedNounPosTags ++ pluralNounPosTags ++ undeterminedNounPosTags

  def getNounLabel(pos: String): Option[String] =
    if(determinedNounPosTags.contains(pos)) Some("NOUN-det")
    else if(pluralNounPosTags.contains(pos)) Some("NOUN-pl")
    else if(undeterminedNounPosTags.contains(pos)) Some("NOUN")
    else None

  def deleteRedundantDeterminers(template: QuestionTemplate[TriggerSlot]): QuestionTemplate[TriggerSlot] = {
    QuestionTemplate(
      template.templateTokens.foldRight(List.empty[TemplateToken[TriggerSlot]]) {
        case (TemplateString(prevWord), TemplateSlot(TriggerSlot("NOUN-det", isTrigger)) :: tail) if prevWord == "<the>".lowerCase =>
          TemplateSlot(TriggerSlot("NOUN-det", isTrigger)) :: tail
        case (TemplateString(prevWord), TemplateString(s) :: tail) if prevWord == "<the>".lowerCase && s == "<obj>".lowerCase =>
          TemplateString("<obj>".lowerCase) :: tail
        case (prevToken, curTokens) => prevToken :: curTokens
      }
    )
  }

  val deleteRedundantDeterminersPhase = phaseFromTemplateProcessor(
    "delete redundant determiners", t => Option(deleteRedundantDeterminers(t))
  )

  def foldDeterminersBeforeIndices(template: QuestionTemplate[TriggerSlot])(indices: Set[Int]): QuestionTemplate[TriggerSlot] =
    QuestionTemplate(
      indices.toVector.sorted.zipWithIndex.map(Function.tupled(_ - _)).foldLeft(template.templateTokens) {
        case (curTemplateTokens, nextIndex) =>
          val isTrigger = curTemplateTokens(nextIndex) match {
            case TemplateSlot(TriggerSlot(_, isTrigger)) => isTrigger
            case _ => ??? // should never happen
          }
          curTemplateTokens.take(nextIndex - 1) ++ (
            TemplateSlot(TriggerSlot("NOUN-det", isTrigger)) ::
              curTemplateTokens.drop(nextIndex + 1)
          )
      }
    )

  val foldDeterminersPhase = TemplatingPhase(
    "fold in determiners", { case TemplatingResult(_, prevAlignments, unalignedQuestions) =>
      val prevTemplates = prevAlignments.map(_._2.template).toSet
      TemplatingResult(
        prevAlignments,
        prevAlignments.map { case (id, qta) =>
          val nounIndicesWithDeterminers = qta.template.templateTokens.zipWithIndex.foldLeft((false, Set.empty[Int])) {
            case ((isPrevDeterminer, curIndices), (TemplateString(s), _)) =>
              if(s == "<the>".lowerCase) (true, curIndices)
              else (false, curIndices)
            case ((true, curIndices), (TemplateSlot(TriggerSlot("NOUN" | "NOUN-pl", _)), index)) =>
              (false, curIndices + index)
            case ((_, curIndices), _) =>
              (false, curIndices)
          }._2
          val indexSetChoicesSorted = nounIndicesWithDeterminers.subsets.toVector.sortBy(-_.size)
          val optionallyCollapsedTemplate =
            indexSetChoicesSorted.iterator
              .map(foldDeterminersBeforeIndices(qta.template))
              .find(prevTemplates.contains)
              .get // will always be present because the last set is empty so it's the original template
          id -> qta.copy(template = optionallyCollapsedTemplate)
        },
        unalignedQuestions)
    }
  )

  val verbLabels = Set("VERB", "VERB-pss")

  // TODO harmlessly pass over non-auxiliaries appearing before the verb so we can see them in later steps
  case class AbstractedVerbTemplate(
    wh: LowerCaseString,
    preSubjectAuxiliaries: List[LowerCaseString],
    subjectAndLaterAuxiliariesOpt: Option[(List[TemplateToken[TriggerSlot]], List[LowerCaseString])],
    verbPOS: String,
    trailingTemplateTokens: List[TemplateToken[TriggerSlot]]) {

    def allAuxiliaries = preSubjectAuxiliaries ++ subjectAndLaterAuxiliariesOpt.map(_._2).foldK
    def isPassive = verbPOS == "VBN" && allAuxiliaries.exists(Inflections.beVerbs.contains)
    def verbLabel = if(isPassive) "VERB-pss" else "VERB"

    def abstractOtherTokens(ts: List[TemplateToken[TriggerSlot]]) = ts.map {
      case TemplateSlot(s) => TemplateSlot(getNounLabel(s.label).fold(s)(s.withLabel))
      case TemplateString(s) if nounPlaceholderWords.contains(s) => TemplateString("<obj>".lowerCase)
      case TemplateString(s) if determiners.contains(s) => TemplateString("<the>".lowerCase)
      case x => x
    }

    def labeledSubjOpt = for {
      (subj, _) <- subjectAndLaterAuxiliariesOpt
    } yield abstractOtherTokens(subj)

    def abstractedTail = abstractOtherTokens(trailingTemplateTokens)

    def verbSlot = TemplateSlot(TriggerSlot(verbLabel, isTrigger = true))
    // def trailingTemplateTokens = trailingWords.map { w =>
    //   if(nounPlaceholderWords.contains(w)) "<obj>".lowerCase
    //   else w
    // }.map(TemplateString(_))

    def whStr =
      if(wh == "what".lowerCase && isDoLast) "what".lowerCase
      else if(wh == "who".lowerCase || wh == "what".lowerCase) "<who/what>".lowerCase
      else wh
    def whTemplateToken = TemplateString(whStr)

    def isDoLast = trailingTemplateTokens.init.lastOption match {
      case Some(TemplateString(w)) => w == "do".lowerCase
      case _ => false
    }

    def abstractedTemplate: QuestionTemplate[TriggerSlot] = {
      val templateTokens = labeledSubjOpt match {
        case None => whTemplateToken :: verbSlot :: abstractedTail
        case Some(subj) =>
          List(whTemplateToken, TemplateString("<aux>".lowerCase)) ++
            subj ++ List(verbSlot) ++ abstractedTail
      }
      QuestionTemplate(templateTokens)
    }

    // assume previous template had no triggers
    def originalTemplate: QuestionTemplate[TriggerSlot] = {
      val templateTokens = {
        (wh :: preSubjectAuxiliaries).map(TemplateString(_)) ++
          subjectAndLaterAuxiliariesOpt.fold(List.empty[TemplateToken[TriggerSlot]]) { case (subj, auxes) =>
            subj ++ auxes.map(TemplateString)
          } ++ (TemplateSlot(TriggerSlot(verbPOS, false)) :: trailingTemplateTokens)
      }
      QuestionTemplate(templateTokens)
    }
  }

  val negOrAuxWords: Set[LowerCaseString] =
    Inflections.auxiliaryVerbs ++ Inflections.negationWords + "n't".lowerCase

  def extractAuxiliaryList(tts: List[TemplateToken[TriggerSlot]]): Option[List[LowerCaseString]] =
    tts.map {
      case TemplateString(s) if negOrAuxWords.contains(s) => Some(s)
      case _ => None
    }.sequence

  def abstractVerbTemplate(template: QuestionTemplate[TriggerSlot]): Option[AbstractedVerbTemplate] = {
    for {
      TemplateString(initWh) :: initTail <- Option(template.templateTokens)
      if template.toList.filter(ts => PosTags.verbPosTags.contains(ts.label)).size == 1 // must have exactly 1 verb (can generalize more later, maybe?)
      (wh, tail) = if(
        initWh == "how".lowerCase && initTail.headOption.exists(_ == TemplateString("much".lowerCase))
      ) ("how much".lowerCase, initTail.tail) else (initWh, initTail)
      (verbPOS, verbSlotIndex) <- tail.collectFirstWithIndex {
        case TemplateSlot(TriggerSlot(v, _)) if PosTags.verbPosTags.contains(v) => v
      }
      trailingTokens = tail.drop(verbSlotIndex + 1)
      beforeVerb = tail.take(verbSlotIndex)
      (aux, subjAndLater) <- extractAuxiliaryList(beforeVerb)
        .map((_, None: Option[(List[TemplateToken[TriggerSlot]], List[LowerCaseString])])).orElse {
        // then try finding subject
        for {
          (subjToken, subjIndex) <- beforeVerb.collectFirstWithIndex {
            case tt @ TemplateString(s) if nounPlaceholderWords.contains(s) => tt
            case tt @ TemplateSlot(TriggerSlot(pos, _)) if getNounLabel(pos).nonEmpty => tt
          }
          (beforeSubj, subj) <- beforeVerb.lift(subjIndex - 1) match {
            case Some(tt @ TemplateString(s)) if determiners.contains(s) =>
              extractAuxiliaryList(beforeVerb.take(subjIndex - 1)).map(_ -> List(tt, subjToken))
            case _ => extractAuxiliaryList(beforeVerb.take(subjIndex)).map(_ -> List(subjToken))
          }
          afterSubj <- extractAuxiliaryList(beforeVerb.drop(subjIndex + 1))
          if beforeSubj.nonEmpty
        } yield (beforeSubj, Some((subj, afterSubj)))
      }
    } yield AbstractedVerbTemplate(wh, aux, subjAndLater, verbPOS, trailingTokens)
  }

  val abstractVerbsPhase = phaseFromTemplateProcessor(
    "abstract verbs", template => abstractVerbTemplate(template).map(_.abstractedTemplate)
  )

  val kindClassifiers = Set("kind", "type", "sort", "kinds", "types", "sorts").map(_.lowerCase)

  val auxTemplateString = TemplateString("<aux>".lowerCase)

  val negationWords = Inflections.negationWords + "n't".lowerCase

  def abstractNounTemplate(template: QuestionTemplate[TriggerSlot]): Option[QuestionTemplate[TriggerSlot]] = {
    for {
      firstSlot <- template.headOption // at least one slot
      if allNounPosTags.contains(firstSlot.label) && !template.exists(ts => PosTags.verbPosTags.contains(ts.label)) // 1st must be noun; no verb slots
      (nounPOS, nounSlotIndex) <- template.templateTokens.collectFirstWithIndex {
        case TemplateSlot(TriggerSlot(n, _)) if allNounPosTags.contains(n) => n
      }
      lastAuxIndexOpt = template.templateTokens.zipWithIndex.collect {
        case (TemplateString(s), i) if Inflections.willVerbs.contains(s) || Inflections.modalVerbs.contains(s) || Inflections.doVerbs.contains(s) || Inflections.haveVerbs.contains(s) || Inflections.beVerbs.contains(s) => i
      }.lastOption
      // isAuxAfterNoun = template.templateTokens.drop(nounSlotIndex + 1).exists {
      //   case TemplateString(s) => Inflections.doVerbs.contains(s) || Inflections.haveVerbs.contains(s) || Inflections.beVerbs.contains(s)
      //   case _ => false
      // }
      abstractedTokens = template.templateTokens.zipWithIndex.flatMap {
        case (TemplateString(s), index) =>
          val newTokenOpt =
            if(kindClassifiers.contains(s)) Some("<kind>".lowerCase)
            else if(negationWords.contains(s)) None
            else if(nounPlaceholderWords.contains(s)) Some("<obj>".lowerCase)
            else if(determiners.contains(s)) Some("<the>".lowerCase)
            else lastAuxIndexOpt match {
              case None => Some(s) // no auxes so the rest is irrelevant anyway
              case Some(auxIndex) =>
                if(index == auxIndex) {
                  if(Inflections.doVerbs.contains(s) || Inflections.willVerbs.contains(s) || Inflections.modalVerbs.contains(s)) Some("<do>".lowerCase)
                  else if(Inflections.beVerbs.contains(s) && (index == 1 || s != "'s".lowerCase)) Some("<be>".lowerCase)
                  else if(Inflections.haveVerbs.contains(s)) Some("<have>".lowerCase)
                  else Some(s) // shouldn't actually happen since this is the aux-index
                } else if(Inflections.auxiliaryVerbs.contains(s)) {
                  Some("<aux>".lowerCase)
                } else Some(s)
            }
          newTokenOpt.map(TemplateString(_))
        case (TemplateSlot(ts), index) => Some(TemplateSlot(ts.withIsTrigger(index == nounSlotIndex)))
      }
      newAbstractedTokens = QuestionTemplate(abstractedTokens).map(ts => getNounLabel(ts.label).fold(ts)(ts.withLabel)).templateTokens
      auxCollapsedAbstractedTokens = newAbstractedTokens.indexOpt(auxTemplateString) match {
        case None => newAbstractedTokens
        case Some(i) => newAbstractedTokens.take(i + 1) ++ newAbstractedTokens.drop(i + 1).filterNot(
          _ == auxTemplateString
        )
      }
    } yield QuestionTemplate(auxCollapsedAbstractedTokens)
  }


  val abstractNounsPhase = phaseFromTemplateProcessor(
    "abstract nouns", abstractNounTemplate
  )

  def abstractPosGroupTemplate(
    getPosLabel: String => Option[String])(
    template: QuestionTemplate[TriggerSlot]
  ): Option[QuestionTemplate[TriggerSlot]] = {
    for {
      firstSlot <- template.headOption
      posLabel <- getPosLabel(firstSlot.label)
      posSlotIndex <- template.templateTokens.findIndex {
        case TemplateSlot(TriggerSlot(pos, _)) => getPosLabel(pos).nonEmpty // first template token must have pos
        case _ => false
      }
      newTemplateTokens = template.templateTokens.zipWithIndex.flatMap {
        case (TemplateString(s), _) =>
          val newTokenOpt =
            if(Inflections.beVerbs.contains(s)) Some("<be>")
            else if(nounPlaceholderWords.contains(s)) Some("<obj>")
            else if((Inflections.negationWords + "n't".lowerCase).contains(s)) None
            else if(determiners.contains(s)) Some("<the>")
            else Some(s.toString)
          newTokenOpt.map(newToken => TemplateString(newToken.lowerCase))
        case (TemplateSlot(TriggerSlot(pos, _)), index) =>
          val slot =
            if(index == posSlotIndex) TriggerSlot(posLabel, true)
            else TriggerSlot(getNounLabel(pos).getOrElse(pos), false)
          Some(TemplateSlot(slot))
      }
      modalRemovedTemplateTokens = newTemplateTokens.foldRight((false, List.empty[TemplateToken[TriggerSlot]])) {
        case (TemplateString(prevToken), (true, tail)) if Inflections.modalVerbs.contains(prevToken) => (false, tail)
        case (TemplateString(s), (_, tail)) if s == "<be>".lowerCase => (true, TemplateString(s) :: tail)
        case (t, (_, tail)) => (false, t :: tail)
      }._2
    } yield QuestionTemplate(modalRemovedTemplateTokens)
  }

  val adjectivePosTags = Set("JJ", "JJR", "JJS")

  // val posTagLabelMapping = Map(
  //   "JJ" -> "ADJ",
  //   "JJR" -> "ADJ-cmp",
  //   "JJS" -> "ADJ-sup",
  //   "CD" -> "NUM",
  //   "EX" -> "THERE",
  //   "PRP$" -> "NOUN-gen")

  // val abstractSimplePosTemplatesPhase = phaseFromTemplateProcessor(
  //   "abstract simple POS templates ", abstractPosGroupTemplate(posTagLabelMapping.get)
  // )

  val adjLabels = Map(
    "JJ" -> "ADJ",
    "JJR" -> "ADJ-cmp",
    "JJS" -> "ADJ-sup")

  val abstractAdjectivesPhase = phaseFromTemplateProcessor(
    "abstract adjectives", abstractPosGroupTemplate(adjLabels.get)
  )

  val numLabels = Map("CD" -> "NUM")

  val abstractNumbersPhase = phaseFromTemplateProcessor(
    "abstract numbers", abstractPosGroupTemplate(numLabels.get)
  )

  val exThereLabels = Map("EX" -> "THERE")

  val abstractExpletiveTherePhase = phaseFromTemplateProcessor(
    "abstract expletive there", abstractPosGroupTemplate(exThereLabels.get)
  )

  val possessivePronounLabels = Map("PRP$" -> "NOUN-gen")

  val abstractPossessivePronounsPhase = phaseFromTemplateProcessor(
    "abstract possessive pronouns", abstractPosGroupTemplate(possessivePronounLabels.get)
  )

  val advLabels = Map(
    "RB" -> "ADV",
    "RBR" -> "ADV-cmp",
    "RBS" -> "ADV-sup")

  val abstractAdverbsPhase = phaseFromTemplateProcessor(
    "abstract adverbs", abstractPosGroupTemplate(advLabels.get)
  )

  val prepLabels = Map("IN" -> "PREP")

  val abstractPrepositionsPhase = phaseFromTemplateProcessor(
    "abstract prepositions", abstractPosGroupTemplate(prepLabels.get)
  )

  def maskOutAllNouns(template: QuestionTemplate[TriggerSlot]) = QuestionTemplate(
    template.templateTokens.map {
      case TemplateSlot(TriggerSlot("NOUN-det", _)) => TemplateString("<obj>".lowerCase)
      case x => x
    }
  )

  val dropNonMatchingMultiPosTemplatesPhase = TemplatingPhase(
    "drop templates with nouns that don't match existing ones with placeholder nouns", {
      case TemplatingResult(_, prevAlignments, prevUnalignedQuestions) =>
        val templateWithPlaceholderCounts = prevAlignments.collect {
          case (_, qta) if qta.template.size == 1 => maskOutAllNouns(qta.template)
        }.groupBy(identity).map(p => p._1 -> p._2.size)
        val templatesWithPlaceholders = templateWithPlaceholderCounts.keySet
        val questionsToDrop = prevAlignments.collect {
          case (id, qta) if !templatesWithPlaceholders.contains(maskOutAllNouns(qta.template)) =>
            id -> qta.sourcedQA
        }
        TemplatingResult(
          previousAlignments = prevAlignments,
          resultAlignments = (prevAlignments -- questionsToDrop.keys),
          unalignedQuestions = prevUnalignedQuestions ++ questionsToDrop)
    }
  )

  val generalizePlaceholderObjectsPhase = TemplatingPhase(
    "generalize <obj>s to NOUN-dets", {
      case TemplatingResult(_, prevAlignments, unalignedQuestions) => TemplatingResult(
        prevAlignments,
        prevAlignments.map { case (id, QuestionTemplateAlignment(sourcedQA, template, alignments)) =>
          val (templateTokens, newAlignments, _) = template.templateTokens.foldRight(
            (List.empty[TemplateToken[TriggerSlot]], List.empty[List[ContiguousSpan]], alignments.reverse)) {
            case (t @ TemplateSlot(_), (tokens, alignments, nextAvailableAlignment :: remainingAlignments)) =>
              (t :: tokens, nextAvailableAlignment :: alignments, remainingAlignments)
            case (t @ TemplateString(s), (tokens, alignments, remainingAlignments)) if s == "<obj>".lowerCase =>
              (TemplateSlot(TriggerSlot("NOUN-det", false)) :: tokens, Nil :: alignments, remainingAlignments)
            case (t @ TemplateString(_), (tokens, alignments, remainingAlignments)) =>
              (t :: tokens, alignments, remainingAlignments)
            // case _ => ??? // should not happen since we have enough alignments to go around
          }
          id -> QuestionTemplateAlignment(sourcedQA, QuestionTemplate(templateTokens), newAlignments)
        },
        unalignedQuestions
      )
    }
  )
}
