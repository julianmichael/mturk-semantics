package turksem.qasrl

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import turkey.HITInfo

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.HasTokens
import nlpdata.util.Text

import turksem.util._

object DataIO {

  def makeQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]]
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false // for now, print everything
      sentenceSB.append(s"${idString}\t${sentenceTokens.mkString(" ")}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valAnswerSpans = for {
              info <- valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              assignment <- info.assignments
              answer <- assignment.response(qaIndex).getAnswer
            } yield answer.spans
            if(valAnswerSpans.size != 2) {
              System.err.println("Warning: don't have 2 validation answers for question. Actual number: " + valAnswerSpans.size)
            } else {
              shouldIncludeSentence = true
              sentenceSB.append("\t")
              sentenceSB.append(wqa.verbIndex.toString + "\t")
              sentenceSB.append(wqa.question + "\t") // question string written by worker
              sentenceSB.append(
                (wqa.answers :: valAnswerSpans).map { spans =>
                  spans
                    .map(span => s"${span.begin}-${span.end}")
                    .mkString(";")
                }.mkString("\t")
              )
              sentenceSB.append("\n")
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }

  // how much and how long must come first so we register them as question prefix
  val whPhrases = List("how much", "how long", "who", "what", "when", "where", "why", "how").map(_.lowerCase)

  // def makeNegExamplesTSV[SID : HasTokens](
  //   ids: List[SID],
  //   writeId: SID => String, // serialize sentence ID for distribution in data file
  //   questionsById: Map[SID, Map[Int, List[String]]])(
  //   implicit inflections: Inflections
  // ) = {
  //   for {

  //   }
  // }

  def makeNegExamplesTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]])(
    implicit inflections: Inflections
  ) = {

    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)

    val (globalWhDist, totalNumQs, localWhDists, numQsForVerb, localVerbFrameSpecs) = {
      val allTuples = for {
        id <- ids
        HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)
        genAssignment <- genAssignments.sortBy(_.workerId)
        (wqa, valResponses) <- genAssignment.response.zip(
          valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil).flatMap(_.assignments).map(_.response).transpose
        )
        if valResponses.forall(_.isAnswer)
        sentenceTokens = genHIT.prompt.id.tokens
        verbInflectedForms <- inflections.getInflectedForms(sentenceTokens(genHIT.prompt.verbIndex).lowerCase)
        wh <- whPhrases.find(s => wqa.question.lowerCase.startsWith(s))
        template = new QASRLStatefulTemplate(new TemplateStateMachine(sentenceTokens, verbInflectedForms))
        goodStates <- template.processStringFully(wqa.question).toOption
        frames = goodStates.toList.collect {
          case QASRLStatefulTemplate.Complete(_, TemplateStateMachine.FrameState(_, _, _, frame)) => frame
        }
      } yield (wqa.question, wh, verbInflectedForms.stem, frames)
      def makeDist[F[_]: Foldable](tuples: F[(String, LowerCaseString, LowerCaseString, List[Frame])]) =
        CategoricalDistribution {
          val counts = NonEmptyList.fromList(
            allTuples.groupBy(_._2).toList.map { case (wh, ts) => wh -> ts.size.toDouble }
          ).get
          val total = counts.map(_._2).sum
          counts.map { case (wh, c) => wh -> (c / total) }
        }

      val globalWh = makeDist(allTuples)
      val localWh = allTuples.groupBy(_._3).map {
        case (verbStem, tuples) => verbStem -> makeDist(tuples)
      }
      val qsForVerb = allTuples.groupBy(_._3).map {
        case (verbStem, tuples) => verbStem -> tuples.size
      }
      val localFrameSpecs = allTuples.groupBy(_._3).map {
        case (verbStem, tuples) => verbStem -> tuples.flatMap(_._4).map(frame =>
          (frame.args, frame.isPassive) // key identifying information for a frame
        )
      }
      (globalWh, allTuples.size, localWh, qsForVerb, localFrameSpecs)
    }

    val sb = new StringBuilder
    val rand = new scala.util.Random(38265192L)
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false // for now, print everything
      sentenceSB.append(s"${idString}\t${sentenceTokens.mkString(" ")}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          val questions = for {
            (wqa, valResponses) <- genAssignment.response.zip(
              valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil).flatMap(_.assignments).map(_.response).transpose
            )
            if valResponses.forall(_.isAnswer)
          } yield wqa.question
          // not sure why this would ever fail, but it happens
          val verbToken = sentenceTokens(genHIT.prompt.verbIndex)
          val verbInflectedFormsOpt = inflections.getInflectedForms(verbToken.lowerCase)
          for(verbInflectedForms <- verbInflectedFormsOpt) {
            val template = new QASRLStatefulTemplate(new TemplateStateMachine(sentenceTokens, verbInflectedForms))
            val whDist = {
              localWhDists.get(verbInflectedForms.stem).fold(globalWhDist)(localDist =>
                localDist.interpolate(globalWhDist, 0.5)
              )
            }
            val suggestedQuestions = QuestionSuggester.getSuggestionsForNegativeSampling(
              template, questions, rand, whDist, localVerbFrameSpecs.get(verbInflectedForms.stem).getOrElse(Nil),
              math.min(questions.size, 3)
            )
            for(negSample <- suggestedQuestions) {
              // pairs of (validation worker ID, validation answer)
              shouldIncludeSentence = true
              sentenceSB.append("\t")
              sentenceSB.append(genHIT.prompt.verbIndex)
              sentenceSB.append("\t")
              sentenceSB.append(negSample)
              sentenceSB.append("\n")
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }

  def makeReadableQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]],
    keepQA: (SID, VerbQA, List[QASRLValidationAnswer]) => Boolean = (
      (_: SID, _: VerbQA, _: List[QASRLValidationAnswer]) => true)
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false
      sentenceSB.append(s"${idString}\t${nlpdata.util.Text.render(sentenceTokens)}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))
            if(valResponses.size != 2) {
              System.err.println("Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size)
            }
            val valAnswers = valResponses.map(_._2)

            if(keepQA(id, wqa, valAnswers)) {
              shouldIncludeSentence = true
              sentenceSB.append(anonymizeWorker(genAssignment.workerId) + "\t") // anonymized worker ID
              sentenceSB.append(s"${Text.normalizeToken(sentenceTokens(wqa.verbIndex))} (${wqa.verbIndex})\t")
              sentenceSB.append(wqa.question + "\t") // question string written by worker
              sentenceSB.append(
                ((Answer(wqa.answers)) :: valResponses.map(_._2)).map { valAnswer =>
                  QASRLValidationAnswer.render(sentenceTokens, valAnswer, genAssignment.response)
                }.mkString("\t")
              )
              sentenceSB.append("\n")
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }
}
