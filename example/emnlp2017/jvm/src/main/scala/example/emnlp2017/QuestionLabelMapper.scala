package example.emnlp2017

import cats.data.NonEmptyList
import cats.implicits._

import qamr.QAData
import qamr.example.AnnotationSetup
import qamr.example.SentenceId

import spacro.tasks.TaskConfig

import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text

import turksem.gapfill.QuestionTemplate
import turksem.gapfill.QuestionTemplateAlignment
import turksem.gapfill.TemplatingPhase
import turksem.gapfill.TemplatingPhases
import turksem.gapfill.TemplatingResult
import turksem.gapfill.TemplateToken
import turksem.gapfill.TemplateString
import turksem.gapfill.TemplateSlot
import turksem.gapfill.TriggerSlot

import turksem.util._

import java.nio.file.Paths

sealed trait PrelimLabel
case class AutoInducedLabel(value: LowerCaseString) extends PrelimLabel
case class QuestionsLabel(qs: Set[LowerCaseString]) extends PrelimLabel

class QuestionLabelMapper(
  setup: AnnotationSetup,
  data: QAData[SentenceId])(
  implicit config: TaskConfig) {

  object Matchers {
    object t {
      def unapply(tt: TemplateToken[TriggerSlot]): Option[String] = tt match {
        case TemplateString(s) => Some(s.toString)
        case _ => None
      }
    }
  }

  import Matchers._

  // detect subj/obj/prep
  // detect predicate
  // detect when/where/why/how
  // detect classifier words
  // detect is/has for noun predicates

  val Wiktionary = new WiktionaryFileSystemService(
    Paths.get("resources/wiktionary")
  )

  import setup.SentenceIdHasTokens
  import setup.isStopword

  implicit val inflections = {
    val tokens = for {
      id <- setup.allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  val phases = new TemplatingPhases[SentenceId]
  import phases._

  // taken from TemplateAnalysis

  // NOTE consider removing adjunct mod filtering, if I want...probably not
  // NOTE fold in genitive clitics too? nah
  val fullPos = posPhase andThen filterAdjunctModPhase
  def postprocess(phase: TemplatingPhase[SentenceId]) =
    phase andThen /* collapseContigProperNounsPhase andThen */ deleteRedundantDeterminersPhase

  // TODO add simple templates for "how adj" and "how adv" and maybe some others to run before adjunct mod filtering

  val whatNounTemplates = Set(
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
         foldDeterminersPhase, dropNoSlotTemplatePhase // dropPOSPhase,
         /*filterInfrequentTemplatesPhase(5), filterBadTemplatesPhase(badTemplates.contains)*/)
  ).flatten

  lazy val templatingPipeline = defaultPipeline

  val sqasById = data.all.map(sqa => sqa.id -> sqa).toMap

  lazy val namedTemplatingResults = templatingPipeline
    .scanLeft(("start", TemplatingResult.initFromQuestions(sqasById))) {
    case (acc, phase) => (phase.name, phase.run(acc._2))
  }
  lazy val templatingResult = namedTemplatingResults.last._2

  lazy val resultAlignmentsBySentenceId = templatingResult.resultAlignments
    .groupBy(_._1.sentenceId)
    .map {
    case (sid, qaPairIdMap) => sid -> qaPairIdMap.values.toList
  }


  def mapLabels(
    sid: SentenceId
  ): (PrelimLabel, Int, Int) => (LowerCaseString, Boolean) = {
    val qtasByQuestion = resultAlignmentsBySentenceId.get(sid).foldK
      .groupBy(_.question.lowerCase)
    val unmappedQuestions = data.sentenceToQAs.get(sid).foldK
      .map(_.question.lowerCase)
      .filterNot(qtasByQuestion.contains)
    // if(unmappedQuestions.nonEmpty) {
    //   println
    //   println(Text.render(sid))
    //   unmappedQuestions.foreach { q => println(s"Untemplated question: $q") }
    // }

    (pl: PrelimLabel, source: Int, target: Int) => pl match {
      case AutoInducedLabel(value) => value -> false
      case QuestionsLabel(qs) =>
        val qtas = qs.toList.flatMap(qtasByQuestion.get).flatten
        // direct case
        val directQTAs = qtas.filter(_.answers.exists(_.contains(target)))
        val (labels, unk): (List[(LowerCaseString, Boolean)], LowerCaseString) = if(directQTAs.nonEmpty) {
          // induce direct labels
          directQTAs.flatMap(getArgLabelOfAnswer(_, source)) -> "unk-direct".lowerCase
        } else {
          // induce indirect labels
          qtas.flatMap(getArgLabelOfQuestionWord(_, source, target)) -> "unk-indirect".lowerCase
        }
        val res = NonEmptyList.fromList(labels).fold(unk -> false)(nel => counts(nel).maxBy(_._2)._1)
        val flipResolvedRes = {
          val classOpt = Some(res._1.toString.dropWhile(_ != '/')).filter(_.nonEmpty)
          val noclassLabel = res._1.toString.takeWhile(_ != '/')
          if(res._2) {
            noclassLabel.lowerCase -> true
          } else classOpt.flatMap(mapIntoMarkerWords) match {
            case Some(l) if noclassLabel == "a1" => l -> false
            case None => noclassLabel.lowerCase -> false
          }
        }
        flipResolvedRes
    }
  }

  val lotsOfPrepositions = Set(
		"aboard", "about", "above", "across", "afore", "after", "against", "ahead", "along", "alongside", "amid",
		"amidst", "among", "amongst", "around", "as", "aside", "astride", "at", "atop", "before",
		"behind", "below", "beneath", "beside", "besides", "between", "beyond", "by", "despite", "down",
		"during", "except", "for", "from", "given", "in", "inside", "into", "near", "next",
		"of", "off", "on", "onto", "opposite", "out", "outside", "over", "pace", "per",
		"round", "since", "than", "through", "throughout", "till", "times", "to", "toward", "towards",
		"under", "underneath", "until", "unto", "up", "upon", "versus", "via", "with ", "within",
		"without"
  ).map(_.lowerCase)

  val markerWords = Set("does", "is", "has", "name").map(_.lowerCase) ++ lotsOfPrepositions

  def mapIntoMarkerWords(s: String): Option[LowerCaseString] = s match {
    case "<do>" | "do" | "does" | "doing" | "done" => Some("does".lowerCase)
    case "<be>" | "is" | "are" | "were" | "am" | "was" | "be" => Some("is".lowerCase)
    case "<have>" | "have" | "has" | "had" => Some("has".lowerCase)
    case "initial" | "name" => Some("name".lowerCase)
    case "kind" | "type" | "sort" | "kinds" | "types" => Some("kind".lowerCase)
    // case "month" => Some("month".lowerCase)
    // case "day" => Some("day".lowerCase)
    // case "year" => Some("year".lowerCase)
    case x if lotsOfPrepositions.contains(x.lowerCase) => Some(x.lowerCase)
    case _ => None
  }

  val detWords = Set(
    "the", "a", "an", "a(n)",
    "this", "these", "their", "his", "her", "our"
  ).map(_.lowerCase)

  def isSkippablePreNounyToken(tt: TemplateToken[TriggerSlot]) = tt match {
    case TemplateString(s) =>
      detWords.contains(s)
    case TemplateSlot(TriggerSlot(label, _)) =>
      label.startsWith("ADJ") || PosTags.adjectivePosTags.contains(label) ||
        label.startsWith("NUM")
  }

  // TODO unused
  val skippablePostNounyTokens = Set("<aux>", "'s", "first", "last", "middle")

  // TODO unused
  def isSkippablePostNounyToken(tt: TemplateToken[TriggerSlot]) = tt match {
    case TemplateString(s) => skippablePostNounyTokens.contains(s)
    case TemplateSlot(_) => false
  }

  // TODO what happened to names?
  def getArgLabelOfAnswerForPredAndClassifier(
    qta: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    classifierOpt: Option[String],
    predInfoPair: (String, Int)
  ): Option[(LowerCaseString, Boolean)] = {
    if(qta.template.show == "what NOUN* ?") {
      Some("which".lowerCase -> false)
    } else qta.template.templateTokens.head.getTemplateString.map(_.value.toString).flatMap {
      case "when" => Some("when" -> false)
      case "where" => Some("where" -> false)
      case "why" => Some("why" -> false)
      case "whose" => Some("whose" -> false)
      case "which" => Some("which" -> false)
      case "how much" => Some("how-much" -> false) // oops I guess...
      case "how" => Some(classifierOpt.fold("how")(cls => s"how/$cls") -> false)
      case "who" | "what" | "<who/what>" =>
        classifierOpt
          .filter(_ == "kind").map(_ -> false)
          .orElse {
          predInfoPair match { case (predLabel, predTokenIndex) =>
            if(predLabel.startsWith("VERB") ||
                 predLabel.startsWith("ADJ") ||
                 PosTags.verbPosTags.contains(predLabel) ||
                 PosTags.adjectivePosTags.contains(predLabel)) {
              val isSubj = !(
                qta.template.templateTokens
                  .take(predTokenIndex) // look before predicate
                  .exists {
                  case TemplateSlot(TriggerSlot(triggerLabel, _)) =>
                    triggerLabel.startsWith("NOUN") || PosTags.nounPosTags.contains(triggerLabel)
                  case TemplateString(s) =>
                    nounPlaceholderWords.contains(s)
                }
              )

              val preLabel = if(isSubj) "a0" else {
                qta.template.templateTokens.init.lastOption
                  .flatMap(_.getTemplateString)
                  .map(_.value.toString)
                  .flatMap(mapIntoMarkerWords)
                  .fold("a1")(_.toString)
              }

              val depassivizedLabel = if(predLabel.endsWith("-pss")) {
                preLabel match {
                  case "by" => "a0"
                  case "a0" => "a1"
                  case x => x
                }
              } else preLabel

              // subj, obj, or prep, as well as classifier
              Some(classifierOpt.fold(depassivizedLabel -> false)(x => s"$depassivizedLabel/$x" -> false))
            } else if(predLabel.startsWith("NOUN") || true) {
              // ending tokens (besides question mark)
              qta.template.templateTokens.init.lastOption
                .flatMap(_.getTemplateString)
                .map(_.value.toString)
                .flatMap(mapIntoMarkerWords)
                .map(_.toString -> false)
                .orElse( // before target, thus inverted. particular case of "do to"
                qta.template.templateTokens
                  .take(predTokenIndex).reverse
                  .dropWhile(isSkippablePreNounyToken).uncons
                  .filter(_._1.getTemplateString.nonEmptyAnd(_.value == "to".lowerCase))
                  .flatMap(_._2.headOption)
                  .flatMap(_.getTemplateString)
                  .map(_.value.toString)
                  .flatMap(mapIntoMarkerWords)
                  .filter(_ == "does".lowerCase)
                  .as("a1" -> true))
                .orElse( // before target, thus inverted. skip "of"
                qta.template.templateTokens
                  .take(predTokenIndex).reverse
                  .dropWhile(isSkippablePreNounyToken).uncons
                  .filter(_._1.getTemplateString.nonEmptyAnd(_.value == "of".lowerCase))
                  .flatMap(_._2.headOption)
                  .flatMap(_.getTemplateString)
                  .map(_.value.toString)
                  .flatMap(mapIntoMarkerWords)
                  .map(_.toString -> false))
                .orElse( // before target, thus inverted.
                qta.template.templateTokens
                  .take(predTokenIndex)
                  .reverse.dropWhile(isSkippablePreNounyToken).headOption
                  .flatMap(_.getTemplateString)
                  .map(_.value.toString)
                  .flatMap(mapIntoMarkerWords)
                  .map(marker => marker.toString -> (marker != "is".lowerCase))) // don't flip preceding "is"
                .orElse( // non-inverted aux appearing directly after verb
                qta.template.templateTokens
                  .drop(predTokenIndex + 1)
                  .lastOption
                  .flatMap(_.getTemplateString)
                  .map(_.value.toString)
                  .flatMap(mapIntoMarkerWords)
                  .map(_.toString -> false))
                .map(p => classifierOpt.fold(p)(cls => s"${p._1}/$cls" -> p._2))
            } else {
              None
            }
          }
        }
      case other =>
        println(s"Missed prefix string: $other")
        None
    }.map { case (label, flipped) => (label.lowerCase, flipped) }
  }

  def getArgLabelOfAnswer(
    qta: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    predIndexInSentence: Int
  ): Option[(LowerCaseString, Boolean)] = {
    val classifierOpt = for {
      tok <- qta.template.templateTokens.lift(1)
      ts <- tok.getTemplateString
      // not a special token. then it's an ext word
      if !ts.value.toString.contains("<") && mapIntoMarkerWords(ts.value.toString).isEmpty
      if !isStopword(ts.value)
    } yield ts.value.toString

    val predInfoPair = qta.template
      .zipWithList(qta.alignments)
      .map(p => p._1 -> p._2.get)
      .templateTokens
      .collectFirstWithIndex {
      case TemplateSlot((TriggerSlot(label, _), span)) if span.exists(_.contains(predIndexInSentence)) =>
        label
    }

    val backupPredInfoPair = qta.template.templateTokens.collectFirstWithIndex {
      case TemplateSlot(TriggerSlot(label, true)) => label
    }

    val res = predInfoPair.flatMap(
      getArgLabelOfAnswerForPredAndClassifier(qta, classifierOpt, _)
    ).orElse(
      backupPredInfoPair.flatMap(getArgLabelOfAnswerForPredAndClassifier(qta, classifierOpt, _))
    )

    // if(res.isEmpty) {
    //   println(f"UNKed direct Who/what question: ${qta.question}%-50s ${qta.template.show}%s")
    //   println(s"\tPredicate: $predInfoPair")
    //   println(s"\tPredicate word: ${qta.sourcedQA.id.sentenceId.tokens(predIndexInSentence)}")
    //   println(s"\tBackup Predicate: $backupPredInfoPair")
    // }

    res


    // finalArgLabelOpt match {
    //   case None =>
    //     // println
    //     // println("Unmatched question:")
    //     // println(qta.question)
    //     // println(qta.template.show)
    //   case Some(label) =>
    //     // if(label._2 == "subj/is") {
    //     //   println
    //     //   println(s"Label: $label")
    //     //   println(s"Question: ${qta.question}")
    //     //   println(s"Template: ${qta.template.show}")
    //     // }
    // }
  }

  def getArgLabelOfQuestionArgForPred(
    qta: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    predInfoPair: (String, Int),
    argInfoPair: (String, Int)
  ): Option[(LowerCaseString, Boolean)] = {
    val (predLabel, predTokenIndex) = predInfoPair
    val (argLabel, argTokenIndex) = argInfoPair

    val isPredVerb = predLabel.startsWith("VERB") || PosTags.verbPosTags.contains(predLabel)
    val isPredPassive = predLabel.endsWith("-pss") || predLabel == "VBN"
    val isArgNoun = argLabel.startsWith("NOUN") || PosTags.nounPosTags.contains(argLabel)

    (argTokenIndex == predTokenIndex + 2).option {
      qta.template.templateTokens.lift(predTokenIndex + 1)
        .flatMap(_.getTemplateString)
        .map(ts => ts.value.toString)
        .flatMap(mapIntoMarkerWords)
        .map(_ -> false)
    }.flatten.orElse(
      (predTokenIndex == argTokenIndex + 2).option {
        qta.template.templateTokens.lift(argTokenIndex + 1)
          .flatMap(_.getTemplateString)
          .map(ts => ts.value.toString)
          .flatMap(mapIntoMarkerWords)
          .map { marker =>
          if(marker == "is".lowerCase && isPredVerb) {
            if(isPredPassive) "a1".lowerCase -> false
            else "a0".lowerCase -> false
          } else marker -> true
        }
      }.flatten
    ).orElse(
      (isPredVerb && isArgNoun && argTokenIndex == predTokenIndex - 1).option(
        if(isPredPassive) "a1".lowerCase -> false
        else "a0".lowerCase -> false
      )
    ).orElse(
      (isPredVerb && isArgNoun && argTokenIndex > predTokenIndex).option(
        qta.template.templateTokens.zipWithIndex
          .drop(predTokenIndex + 1)
          .dropWhile(p => isSkippablePreNounyToken(p._1))
          .headOption
          .filter(_._2 == argTokenIndex && !isPredPassive)
          .as("a1".lowerCase -> false)
      ).flatten
    )
    // TODO:
    // 1. prep before noun appearing right after pred
    // 2. object of pred-verb
    // 3. etc.
    // Some("unko-dunko".lowerCase -> false)
    // None
  }

  def getArgLabelOfQuestionWord(
    qta: QuestionTemplateAlignment[SentenceId, TriggerSlot],
    predIndexInSentence: Int,
    questionWordIndexInSentence: Int
  ): Option[(LowerCaseString, Boolean)] = {

    val questionArgInfoPair = qta.template
      .zipWithList(qta.alignments)
      .map(p => p._1 -> p._2.get)
      .templateTokens
      .collectFirstWithIndex {
      case TemplateSlot((TriggerSlot(label, _), span)) if span.exists(_.contains(questionWordIndexInSentence)) => label
    }

    if(questionArgInfoPair.isEmpty) {
      // println(f"UNKed indirect Who/what question: ${qta.question}%-50s ${qta.template.show}%s")
      // println(s"\tPredicate word: ${qta.sourcedQA.id.sentenceId.tokens(predIndexInSentence)}")
      // println(s"\tQuestion arg word: ${qta.sourcedQA.id.sentenceId.tokens(questionWordIndexInSentence)}")
      // println(s"\t== Question word not found in question ==")
    }

    questionArgInfoPair.flatMap { questionArgInfo =>
      val predInfoPair = qta.template
        .zipWithList(qta.alignments)
        .map(p => p._1 -> p._2.get)
        .templateTokens
        .collectFirstWithIndex {
        case TemplateSlot((TriggerSlot(label, _), span)) if span.exists(_.contains(predIndexInSentence)) =>
          label
      }

      val backupPredInfoPair = qta.template.templateTokens.collectFirstWithIndex {
        case TemplateSlot(TriggerSlot(label, true)) => label
      }

      val res = predInfoPair
        .flatMap(getArgLabelOfQuestionArgForPred(qta, _, questionArgInfo))
        .orElse(backupPredInfoPair.flatMap(getArgLabelOfQuestionArgForPred(qta, _, questionArgInfo)))
        .orElse(predInfoPair.flatMap(getArgLabelOfQuestionArgForPred(qta, questionArgInfo, _))
                  .map(p => p._1 -> !p._2))
        .orElse(backupPredInfoPair.flatMap(getArgLabelOfQuestionArgForPred(qta, questionArgInfo, _))
                  .map(p => p._1 -> !p._2))

      if(res.isEmpty) {
        // println(f"UNKed indirect Who/what question: ${qta.question}%-50s ${qta.template.show}%s")
        // println(s"\tPredicate: $predInfoPair")
        // println(s"\tPredicate word: ${qta.sourcedQA.id.sentenceId.tokens(predIndexInSentence)}")
        // println(s"\tBackup Predicate: $backupPredInfoPair")
        // println(s"\tQuestion arg word: ${qta.sourcedQA.id.sentenceId.tokens(questionWordIndexInSentence)}")
        // println(s"\tQuestion is in template: ${qta.sourcedQA.id.sentenceId.tokens(questionWordIndexInSentence)}")
      }

      res
    }

  }

    // val labelGroupsByQuestion = resultAlignmentsBySentenceId.get(sid).foldK
    //   .flatMap(qta => getQuestionPredAndLabelOpt(qta).map(qta.question.lowerCase -> _))
    //   .groupSecondByFirst[LowerCaseString, (ContiguousSpan, LowerCaseString, Boolean)]
    //   .map { case (question, labels) => question -> labels.map(_._2).modes.head }
    //   .toMap
    // val res = labels.zip(
    //   labels.map {
    //     case AutoInducedLabel(value) => value
    //     case QuestionsLabel(qs) =>
    //       qs.toList
    //         .flatMap(labelGroupsByQuestion.get)
    //         .modes
    //         .headOption
    //         .getOrElse("unk-dir".lowerCase)
    //   }
    // ).toMap
    // // println; println
    // // res.foreach { case (label, value) =>
    // //   println(f"$value%-15s $label%s")
    // // }
    // res

  def getLabeledQAsForId(sid: SentenceId) = {
    resultAlignmentsBySentenceId.get(sid).map { qtas =>
      qtas.flatMap(qta =>
        getQuestionPredAndLabelOpt(qta).map(qta -> _)
      )
    }.foldK
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

  // TODO fix "is" classifiers, implement "how" classifiers, solve other weird issues in the templating pipeline, etc.
  def getQuestionPredAndLabelOpt(qta: QuestionTemplateAlignment[SentenceId, TriggerSlot]): Option[(ContiguousSpan, LowerCaseString, Boolean)] = {
    getTriggerSpan(qta).flatMap { predSpan =>
      val label = getTriggerSlot(qta.template).label
      val argLabelOpt: Option[(ContiguousSpan, String, Boolean)] = if(label.startsWith("VERB") || label.startsWith("ADJ") || PosTags.verbPosTags.contains(label)) {
        val verbOrAdjLabel = qta.template.templateTokens.head match {
          case TemplateSlot(_) => None // should not happen
          case TemplateString(s) => s.toString match {
            case "when" => Some("when")
            case "where" => Some("where")
            case "why" => Some("why")
            case "how" =>
              val label = NonEmptyList.fromList(
                qta.template.templateTokens.tail.map(_.getTemplateString)
                  .takeWhile(_.nonEmpty).map(_.get.value.toString)
                  .takeWhile(s => !s.contains("<"))
              ).map(_.toList.mkString("-")).fold("how")(x => s"how-$x")
              Some(label)
            case "who" | "what" | "<who/what>" =>
              val classifierOpt = for {
                tok <- qta.template.templateTokens.lift(1)
                ts <- tok.getTemplateString
                if !ts.value.toString.contains("<") // not a special token. then it's an ext word
              } yield ts.value.toString
              val isObj = (
                qta.template.templateTokens
                  .takeWhile(!_.getTemplateSlot.nonEmptyAnd(_.slot.isTrigger))
                  .flatMap(_.getTemplateString).map(_.value.toString)
                  .exists(s => s == "<aux>") || qta.template.templateTokens
                  .takeWhile(!_.getTemplateSlot.nonEmptyAnd(_.slot.isTrigger))
                  .exists(tt =>
                  tt.getTemplateSlot.map(_.slot.label).nonEmptyAnd(label => label.startsWith("NOUN") || PosTags.nounPosTags.contains(label)) ||
                    tt.getTemplateString.map(_.value).nonEmptyAnd(nounPlaceholderWords.contains)
                )
              ) || label.endsWith("-pss")
              if(!isObj) Some {
                classifierOpt.fold("subj")(x => s"subj/$x")
              } else Some {
                // ending tokens
                val label = NonEmptyList.fromList(
                  qta.template.templateTokens
                    .reverse.tail.takeWhile(_.isTemplateString).map(_.getTemplateString.get).filterNot(ts => ts.value.toString.contains("<") || nounPlaceholderWords.contains(ts.value))
                ).fold("obj")(_.map(_.value.toString).toList.reverse.mkString("-"))
                classifierOpt.fold(label)(x => s"$label/$x")
              }
            // subj, obj, or prep, as well as classifier
            case _ =>
              None
          }
        }
        verbOrAdjLabel.map(label => (predSpan, label, false))
      } else if(label.startsWith("NOUN") || true) { // all other labels actually work here too I think. again it's just a best effort anyway
        qta.template.templateTokens.head match {
          case TemplateSlot(_) => None // should not happen
          case TemplateString(s) => s.toString match {
            case "when" => Some((predSpan, "when", false))
            case "why" => Some((predSpan, "why", false))
            case "whose" => Some((predSpan, "whose", false))
            case "which" => Some((predSpan, "which", false))
            case "how" =>
              val label = NonEmptyList.fromList(
                qta.template.templateTokens.tail.map(_.getTemplateString)
                  .takeWhile(_.nonEmpty).map(_.get.value.toString)
                  .takeWhile(s => !s.contains("<"))
              ).map(_.toList.mkString("-")).fold("how")(x => s"how-$x")
              Some((predSpan, label, false))
            case "where" | "who" | "what" | "<who/what>" =>
              val classifierOpt = for {
                tok <- qta.template.templateTokens.lift(1)
                ts <- tok.getTemplateString
                if !ts.value.toString.contains("<") // not a special token. then it's an ext word
              } yield ts.value.toString
              // ending tokens
              val argLabelTuple = NonEmptyList.fromList(
                qta.template.templateTokens
                  .reverse.tail.takeWhile(_.isTemplateString)
                  .map(_.getTemplateString.get.value.toString match {
                         case "<have>" => "has"
                         case "<do>" => "does"
                         case x => x
                       }
                ).filterNot(s => s.contains("<") || s == "the" || s == "a(n)" || s == "'s")
              ).map(ls => (predSpan, ls.toList.reverse.mkString("-"), false)).orElse( // before target noun; inverted relation
                NonEmptyList.fromList(
                  qta.template.templateTokens
                    .takeWhile(!_.getTemplateSlot.nonEmptyAnd(_.slot.isTrigger))
                    .reverse
                    .dropWhile(_.isTemplateSlot)
                    .takeWhile(_.getTemplateString.nonEmptyAnd(ts => !ts.value.toString.contains("<") && !whWords.contains(ts.value) && !classifierOpt.contains(ts.value.toString)))
                    .map(_.getTemplateString.get.value.toString)
                    .filterNot(s => s == "the" || s == "a(n)" || s == "'s")
                ).map(ls => (predSpan, ls.toList.reverse.mkString("-"), true))
              ).orElse( // use aux verb, again inverted
                qta.template.templateTokens.find(_.getTemplateString.map(_.value.toString).nonEmptyAnd(s => s.contains("<") && s != "<aux>")).flatMap(tt =>
                  (tt.getTemplateString.get.value.toString match {
                     case "<be>" => Some("is")
                     case "<have>" => Some("has")
                     case "<do>" => Some("does")
                     case _ => None
                   }).map(label => (predSpan, label , true))
                )
              ).orElse( // non-inverted aux appearing after verb
                qta.template.templateTokens
                  .dropWhile(!_.getTemplateSlot.nonEmptyAnd(_.slot.isTrigger)).tail
                  .takeWhile(_.getTemplateString.nonEmpty)
                  .flatMap(_.getTemplateString.map(_.value.toString))
                  .filter(s => s.contains("<") && s != "<aux>")
                  .headOption.flatMap(s =>
                  (s match {
                     case "<be>" => Some("is")
                     case "<have>" => Some("has")
                     case "<do>" => Some("does")
                     case _ => None
                   }).map(label => (predSpan, label , true)))
              )
              classifierOpt.fold(argLabelTuple)(cls =>
                argLabelTuple.map(t => (t._1, s"${t._2}/$cls", t._3))
              ): Option[(ContiguousSpan, String, Boolean)]
            // subj, obj, or prep, as well as classifier
            case _ =>
              None
          }
        }
      } else {
        None
      }
      val finalArgLabelOpt = argLabelOpt.orElse(
        (whatNounTemplates.contains(qta.template)).option(
          (predSpan, "which", false)
        )
      )
      finalArgLabelOpt match {
        case None =>
          // println
          // println("Unmatched question:")
          // println(qta.question)
          // println(qta.template.show)
        case Some(label) =>
          // if(label._2 == "subj/is") {
          //   println
          //   println(s"Label: $label")
          //   println(s"Question: ${qta.question}")
          //   println(s"Template: ${qta.template.show}")
          // }
      }
      finalArgLabelOpt.map(tuple =>
        (tuple._1, tuple._2.lowerCase, tuple._3)
      )
    }
  }
}
