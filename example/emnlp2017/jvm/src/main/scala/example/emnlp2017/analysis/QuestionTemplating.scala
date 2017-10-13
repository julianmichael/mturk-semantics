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
}
