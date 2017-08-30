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

object QuestionTemplating {

  import Datasets._
  import Reinflection._
  import TemplateToken._

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

  val preStemAuxes = Set(
    "do", "did", "does", "to"
  ).map(_.lowerCase) ++ Inflections.willVerbs ++ Inflections.modalVerbs ++ Inflections.wouldVerbs

  def chooseStemOrNon3rdSgPresent(qTokens: Vector[LowerCaseString], verbIndex: Int): String =
    if(qTokens.take(verbIndex).exists(preStemAuxes.contains)) "VB" else "VBP"

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
      val alignedSentenceSpans = finalAlignments.map(i => ContiguousSpan(i, i))
      val templateTokens = qTokens.toList
        .map(tok => TemplateString(tok.lowerCase))
        .updated(alignedQIndex, TemplateSlot(pos))
      QuestionTemplateAlignment(sqa, QuestionTemplate(templateTokens), List(alignedSentenceSpans))
    }
  }

  val nounPlaceholderWords = Set(
    "someone", "something",
    "I", "we", "you", "he", "she", "it", "they",
    "this", "that", "those", "these",
    "him", "her", "me", "us", "them"
  ).map(_.lowerCase)

  case class AbstractedVerbTemplate(
    wh: LowerCaseString,
    auxiliaries: List[LowerCaseString],
    subjectAndLaterAuxiliaries: List[LowerCaseString],
    verbPOS: String,
    trailingWords: List[LowerCaseString]) {

    def postSubjectAuxiliaries =
      if(subjectAndLaterAuxiliaries.isEmpty) Nil else subjectAndLaterAuxiliaries.tail
    def allAuxiliaries = auxiliaries ++ postSubjectAuxiliaries
    def isPassive = verbPOS == "VBN" && allAuxiliaries.exists(Inflections.beVerbs.contains)
    def verbLabel = if(isPassive) "VERB-pss" else "VERB"

    def verbSlot = TemplateSlot(verbLabel)
    def trailingTemplateTokens = trailingWords.map { w =>
      if(nounPlaceholderWords.contains(w)) "<obj>".lowerCase
      else w
    }.map(TemplateString(_))

    def whStr =
      if(wh == "what".lowerCase && isDoLast) "what".lowerCase
      else if(wh == "who".lowerCase || wh == "what".lowerCase) "<who/what>".lowerCase
      else wh
    def whTemplateToken = TemplateString(whStr)

    def isDoLast = trailingWords.init.lastOption match {
      case Some(w) => w == "do".lowerCase
      case _ => false
    }

    def abstractedTemplate: QuestionTemplate[String] = {
      val templateTokens = subjectAndLaterAuxiliaries match {
        case Nil => whTemplateToken :: verbSlot :: trailingTemplateTokens
        case _ => List(
          whTemplateToken, TemplateString("<aux>".lowerCase),
          TemplateString("<subj>".lowerCase), verbSlot
        ) ++ trailingTemplateTokens
      }
      QuestionTemplate(templateTokens)
    }

    def originalTemplate: QuestionTemplate[String] = {
      val templateTokens = {
        (wh :: (auxiliaries ++ subjectAndLaterAuxiliaries)).map(TemplateString(_)) ++
          (TemplateSlot(verbPOS) :: trailingWords.map(TemplateString(_)))
      }
      QuestionTemplate(templateTokens)
    }
  }

  val negOrAuxWords: Set[LowerCaseString] =
    Inflections.auxiliaryVerbs ++ Inflections.negationWords + "n't".lowerCase

  def extractAuxiliaryList(tts: List[TemplateToken[String]]): Option[List[LowerCaseString]] =
    tts.map {
      case TemplateString(s) if negOrAuxWords.contains(s) => Some(s)
      case _ => None
    }.sequence

  // start out using old template formalism, even though it may not be the best fit
  def abstractVerbTemplate(template: QuestionTemplate[String]): Option[AbstractedVerbTemplate] = {
    // assume 1 slot, and that it's a verb
    for {
      TemplateString(initWh) :: initTail <- Option(template.templateTokens)
      (wh, tail) = if(
        initWh == "how".lowerCase && initTail.headOption.exists(_ == TemplateString("much".lowerCase))
      ) ("how much".lowerCase, initTail.tail) else (initWh, initTail)
      (verbPOS, verbSlotIndex) <- tail.collectFirstWithIndex {
        case TemplateSlot(v) if PosTags.verbPosTags.contains(v) => v
      }
      trailingWords <- tail.drop(verbSlotIndex + 1).map {
        case TemplateString(s) => Some(s)
        case _ => None
      }.sequence
      beforeVerb = tail.take(verbSlotIndex)

      (aux, subjAndLater) <- extractAuxiliaryList(beforeVerb)
        .map((_, List.empty[LowerCaseString])).orElse {
        // then try finding subject
        for {
          (subj, subjIndex) <- beforeVerb.collectFirstWithIndex {
            case TemplateString(s) if nounPlaceholderWords.contains(s) => s
          }
          beforeSubj <- extractAuxiliaryList(beforeVerb.take(subjIndex))
          afterSubj <- extractAuxiliaryList(beforeVerb.drop(subjIndex + 1))
          if beforeSubj.nonEmpty
        } yield (beforeSubj, subj :: afterSubj)
      }
    } yield AbstractedVerbTemplate(wh, aux, subjAndLater, verbPOS, trailingWords)
  }

  val kindClassifiers = Set("kind", "type", "sort").map(_.lowerCase)

  val determiners = Set("the", "a", "an").map(_.lowerCase)

  val determinedNounPosTags = Set("NNP", "PRP", "DT")
  val undeterminedNounPosTags = Set("NNPS", "NN", "NNS")
  val allNounPosTags = determinedNounPosTags ++ undeterminedNounPosTags

  val auxTemplateString = TemplateString("<aux>".lowerCase)

  val negationWords = Inflections.negationWords + "n't".lowerCase

  def abstractNounTemplate(template: QuestionTemplate[String]): Option[QuestionTemplate[String]] = {
    // assume 1 slot, and that it's a noun
    for {
      (nounPOS, nounSlotIndex) <- template.templateTokens.collectFirstWithIndex {
        case TemplateSlot(n) if allNounPosTags.contains(n) => n
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
            else lastAuxIndexOpt match {
              case None => Some(s) // no auxes so the rest is irrelevant anyway
              case Some(auxIndex) =>
                if(index == auxIndex) {
                  if(Inflections.doVerbs.contains(s) || Inflections.willVerbs.contains(s) || Inflections.modalVerbs.contains(s)) Some("<do>".lowerCase)
                  else if(Inflections.beVerbs.contains(s)) Some("<be>".lowerCase)
                  else if(Inflections.haveVerbs.contains(s)) Some("<have>".lowerCase)
                  else Some(s) // shouldn't actually happen since this is the aux-index
                } else if(Inflections.auxiliaryVerbs.contains(s)) {
                  Some("<aux>".lowerCase)
                } else Some(s)
            }
          newTokenOpt.map(TemplateString(_))
        case (TemplateSlot(n), _) => Some(TemplateSlot(n))
      }
      wordBeforeNoun <- template.templateTokens(nounSlotIndex - 1) match {
        case TemplateString(s) => Some(s)
        case _ => None
      }
      newAbstractedTokens = if(determiners.contains(wordBeforeNoun)) {
        abstractedTokens.take(nounSlotIndex - 1) ++ (
          TemplateSlot("DNOUN") :: abstractedTokens.drop(nounSlotIndex + 1)
        )
      } else {
        val nounLabel =
          if(determinedNounPosTags.contains(nounPOS)) "DNOUN"
          else "NOUN"
        abstractedTokens.take(nounSlotIndex) ++ (
          TemplateSlot(nounLabel) :: abstractedTokens.drop(nounSlotIndex + 1)
        )
      }
      auxCollapsedAbstractedTokens = newAbstractedTokens.indexOpt(auxTemplateString) match {
        case None => newAbstractedTokens
        case Some(i) => newAbstractedTokens.take(i + 1) ++ newAbstractedTokens.drop(i + 1).filterNot(
          _ == auxTemplateString
        )
      }
    } yield QuestionTemplate(auxCollapsedAbstractedTokens)
  }

  val adjectivePosTags = Set("JJ", "JJR", "JJS")

  def abstractAdjectiveTemplate(template: QuestionTemplate[String]): Option[QuestionTemplate[String]] = {
    // assume 1 slot, and that it's an adjective
    for {
      (adjPOS, adjSlotIndex) <- template.templateTokens.collectFirstWithIndex {
        case TemplateSlot(n) if adjectivePosTags.contains(n) => n
      }
      newTemplateTokens = template.templateTokens.flatMap {
        case TemplateString(s) =>
          val newTokenOpt =
            if(Inflections.beVerbs.contains(s)) Some("<be>")
            else if(nounPlaceholderWords.contains(s)) Some("<obj>")
            else if((Inflections.negationWords + "n't".lowerCase).contains(s)) None
            else Some(s.toString)
          newTokenOpt.map(newToken => TemplateString(newToken.lowerCase))
        case TemplateSlot(_) =>
          val adjLabel =
            if(adjPOS == "JJ") "ADJ"
            else if(adjPOS == "JJR") "ADJ-cmp"
            else "ADJ-sup"
          Some(TemplateSlot(adjLabel))
      }
    } yield QuestionTemplate(newTemplateTokens)
  }

  def abstractNumberTemplate(template: QuestionTemplate[String]): Option[QuestionTemplate[String]] = {
    // TODO
    None
  }

  def templatizeQuestionSingleWordWithMostAbstraction(
    sqa: SourcedQA[SentenceId]
  ): Option[QuestionTemplateAlignment[String]] = {
    for {
      qta <- templatizeQuestionSingleWordWithPOS(sqa)
      abstTemplate <- abstractVerbTemplate(qta.template).map(_.abstractedTemplate).orElse(
        abstractNounTemplate(qta.template)
      ).orElse(abstractAdjectiveTemplate(qta.template)
      ).orElse(abstractNumberTemplate(qta.template))
    } yield qta.copy(template = abstTemplate)
  }
}
