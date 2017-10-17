package example.interactive

import example.emnlp2017._
import example.emnlp2017.analysis._

import turksem.iqa._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text

import cats.implicits._

object QuestionGuessingSandbox {
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

  lazy val templateAnalysis = TemplateAnalysis.getDefaultAnalysis("tiny", Datasets.ptbDev)
  lazy val qtasByInteractiveTemplate = templateAnalysis.alignmentsWithLabeledAnswers.flatMap {
    case (id, (qta, matchLabel, answerIndex)) =>
      val sentence = id.sentenceId.tokens
      val answerArg: GappableArgument = matchLabel match {
        // TODO the match labels are not quite getting the to-verbs I don't think
        case "NOUN" => Noun(NounConstraint.DetOrVerb)
        case "VERB" => ToVerb
      }
      val answerSpec = SentenceWord(answerIndex)
      import AdjectiveKind._
      val adjLabels = Map("ADJ" -> Regular, "ADJ-cmp" -> Comparative, "ADJ-sup" -> Superlative)
      // import NounConstraint._
      // val nounLabels = Map("NOUN" -> Bare, "NOUN-det" -> DetOrVerb, // TODO rest

      // println("Working on template alignment:")
      // println(Text.render(sentence))
      // println(qta.sourcedQA.question)
      // println(qta.template.show)
      // println(qta.template)
      // println(qta.alignments)
      // println(qta.alignments.map(_.map(span => Text.renderSpan(sentence, span.indices)).mkString(";")).mkString("\t"))
      // println

      val (questionTokens, argSpecOpts, _) = qta.template.templateTokens
        .filterNot(_ == TemplateString("?".lowerCase))
        .foldRight((List.empty[Token], List.empty[Option[ArgumentSpecifier]], qta.alignments.reverse)) {
        case (TemplateString(s), (tokens, args, alignments)) =>
          if(s == "who".lowerCase) (StringToken("what".lowerCase) :: tokens, args, alignments)
          else if(s == "<who/what>".lowerCase) (StringToken("what".lowerCase) :: tokens, args, alignments)
          else if(s == "<be>".lowerCase) (Copula :: tokens, args, alignments)
          else if(s == "<aux>".lowerCase) (StringToken("<aux>".lowerCase) :: tokens, args, alignments)
          else if(lotsOfPrepositions.contains(s)) (Preposition :: tokens, Some(ExternalWord(s)) :: args, alignments)
          else (StringToken(s) :: tokens, args, alignments)
        case (TemplateSlot(TriggerSlot(label, _)), (tokens, args, nextAlignment :: alignments)) => label match {
          case "VERB" => (
            AlignedVerb(VerbVoice.Active) :: tokens,
            nextAlignment.headOption.map(s => SentenceWord(s.begin)) :: args,
            alignments)
          case "VERB-pss" => (
            AlignedVerb(VerbVoice.Passive) :: tokens,
            nextAlignment.headOption.map(s => SentenceWord(s.begin)) :: args,
            alignments)
          case "NOUN-det" => (
            Noun(NounConstraint.DetOrVerb) :: tokens,
            nextAlignment.headOption.map(s => SentenceWord(s.begin)) :: args,
            alignments)
          case "PREP" => (
            Preposition :: tokens,
            Some(ExternalWord(sentence(nextAlignment.head.begin).lowerCase)) :: args,
            alignments)
          case label if adjLabels.keySet.contains(label) => (
            Adjective(adjLabels(label)) :: tokens,
            nextAlignment.headOption.map(s => SentenceWord(s.begin)) :: args,
            alignments)
          case label => ( // don't expect the result to be a proper template
            StringToken(("xxx" + label + "xxx").lowerCase) :: tokens,
            args, // skip placing the arg since we're doing a string token in this case
            alignments)
        }
      }
      val qaTemplate = QATemplate(questionTokens, answerArg)
      if(qaTemplatesToAlignments.keySet.contains(qaTemplate)) {
        Some(qta -> ArgSpecifiedQATemplate(qaTemplate, argSpecOpts, answerSpec))
      } else {
        // if(!qaTemplate.toString.contains("xxx")) {
        //   println("Bad QA Template:")
        //   println(qaTemplate)
        //   println
        // }
        None
      }
  }

  lazy val templateCountSmoothingConstant = 2.0
  lazy val templatePseudoCounts = qtasByInteractiveTemplate
    .map { case (_, ArgSpecifiedQATemplate(qaTemplate, _, _)) => qaTemplate }
    .foldLeft(Map.empty[Template, Double]) { case (counts, qaTemplate) =>
      val templatesRepped = qaTemplatesToAlignments(qaTemplate).map(_.propositionTemplate)
      val pseudoCountForEach = 1.0 / templatesRepped.size
      val newTemplateCounts = templatesRepped.map(_ -> pseudoCountForEach).toMap
      counts |+| newTemplateCounts
  } |+| allTemplates.all.map(_ -> templateCountSmoothingConstant).toMap

  lazy val allPossibleCorrespondences = allTemplates.groups.flatMap { group =>
    for {
      t1 <- group
      t1TriggerIndex <- allTemplates.getTriggerIndex(t1).toList
      t2 <- group
      t2TriggerIndex <- allTemplates.getTriggerIndex(t2).toList
      correspondence <- TemplateArgumentCorrespondence.generateAllPossibleCorrespondences(
        t1, t1TriggerIndex, t2, t2TriggerIndex
      )
    } yield correspondence
  }.toSet

  def getCorrespondences(
    qta1: QuestionTemplateAlignment[TriggerSlot], argSpecTemplate1: ArgSpecifiedQATemplate,
    qta2: QuestionTemplateAlignment[TriggerSlot], argSpecTemplate2: ArgSpecifiedQATemplate
  ): List[TemplateArgumentCorrespondence] = for {
    // get the possible alignments of these QA templates to proposition templates
    alignedTemplate1 <- qaTemplatesToAlignments(argSpecTemplate1.qaTemplate)
    triggerIndex1 <- allTemplates.getTriggerIndex(alignedTemplate1.propositionTemplate).toList
    alignedTemplate2 <- qaTemplatesToAlignments(argSpecTemplate2.qaTemplate)
    triggerIndex2 <- allTemplates.getTriggerIndex(alignedTemplate2.propositionTemplate).toList
    // must be different templates
    if alignedTemplate1.propositionTemplate != alignedTemplate2.propositionTemplate
    // templates must agree on the trigger
    if alignedTemplate1.propositionTemplate.arguments(triggerIndex1) ==
       alignedTemplate2.propositionTemplate.arguments(triggerIndex2)
    // construct mappings from (prop. template arg index) -> (corresponding arg spec in QA)
    template1ArgSpecs = (
      alignedTemplate1.questionAlignments.zip(argSpecTemplate1.questionArgSpecs) :+ (
        alignedTemplate1.answerAlignment -> Some(argSpecTemplate1.answerArgSpec)
      )
    ).toMap
    template2ArgSpecs = (
      alignedTemplate2.questionAlignments.zip(argSpecTemplate2.questionArgSpecs) :+ (
        alignedTemplate2.answerAlignment -> Some(argSpecTemplate2.answerArgSpec)
      )
    ).toMap
    // the arg specs chosen for the triggers must align
    if template1ArgSpecs(triggerIndex1) == template2ArgSpecs(triggerIndex2)
    (template1ArgIndex, template1ArgSpec) <- template1ArgSpecs.toList
    if template1ArgIndex != triggerIndex1
    (template2ArgIndex, template2ArgSpec) <- template2ArgSpecs.toList
    if template2ArgIndex != triggerIndex2
    // and finally, they must have specified the same argument
    if template1ArgSpec == template2ArgSpec
  } yield TemplateArgumentCorrespondence.make(
    alignedTemplate1.propositionTemplate -> alignedTemplate2.propositionTemplate,
    template1ArgIndex -> template2ArgIndex
  )

  def getAllTemplateCorrespondences = for {
    (qta1, argSpecTemplate1) :: tail <- qtasByInteractiveTemplate.toList.tails.filter(_.nonEmpty)
    (qta2, argSpecTemplate2) <- tail
    correspondence <- getCorrespondences(qta1, argSpecTemplate1, qta2, argSpecTemplate2)
  } yield correspondence

  lazy val templateCorrespondencePseudoCounts =
    getAllTemplateCorrespondences.toList.groupBy(identity).map(p => p._1 -> p._2.size.toDouble)
}

case class ArgSpecifiedQATemplate(
  qaTemplate: QATemplate,
  questionArgSpecs: List[Option[ArgumentSpecifier]],
  answerArgSpec: SentenceWord)
