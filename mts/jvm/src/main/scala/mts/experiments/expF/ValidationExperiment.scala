package mts.experiments.expF

import mts.analysis._
import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

class ValidationExperiment(implicit config: TaskConfig) {
  val experimentName = "f_validation"

  val answerValidationHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and a list of questions,
      highlight the part of the sentence that answers the question.
    """.trim,
    reward = 0.20,
    keywords = "language,english,question answering")

  lazy val sentenceApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(FileManager.getCoNLLSentence(path).get)
  }

  val sampleValidationPrompt = ValidationPrompt(
    sentences.head._1, List(
      SourcedQAPair("", "Who is awesome?", Set(1, 2, 3, 4)),
      SourcedQAPair("", "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val avTaskSpec = TaskSpecification[ValidationPrompt, AnswerValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFAnswerValidationTaskKey, answerValidationHITType, sentenceApiFlow, sampleValidationPrompt,
    frozenHITTypeId = Some("35NE15Q62B2FVT9IPT6KFM3QDZDT7K"))

  val questionValidationHITType = HITType(
    title = s"Simplify questions about a sentence",
    description = s"""
      You'll be given a sentence and a list of questions (and their answers).
      Revise and simplify the questions while preserving their answers.
    """.trim,
    reward = 0.30,
    keywords = "language,english,question answering")

  lazy val qvTaskSpec = TaskSpecification[ValidationPrompt, QuestionValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFQuestionValidationTaskKey, questionValidationHITType, sentenceApiFlow, sampleValidationPrompt,
    frozenHITTypeId = Some("3SGI3JPMZ02SE0U77QDB3KAL5GWFBK"))

  val longAnswerValidationHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and a list of questions,
      highlight the longest part the sentence that correctly answers the question.
    """.trim,
    reward = 0.20,
    keywords = "language,english,question answering")
  lazy val lavTaskSpec = TaskSpecification[ValidationPrompt, AnswerValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFLongAnswerValidationTaskKey, longAnswerValidationHITType, sentenceApiFlow, sampleValidationPrompt,
    frozenHITTypeId = Some("3Z1ME8JRSHB5UIZ6KTSW1YPHF2NCOA"))

  import config.actorSystem
  lazy val server = new Server(
    List(avTaskSpec, qvTaskSpec, lavTaskSpec))

  // get all of the questions to validate from expE

  import expE.{QuestionWordExperiment, QAGenPrompt, QAGenResponse}
  lazy val experimentE = new QuestionWordExperiment
  lazy val expEHITInfos = experimentE.loadQAGenData.map(GenIso.fields[HITInfo[QAGenPrompt, QAGenResponse]].reverseGet)

  lazy val questionInfos = for {
    HITInfo(hit, assignments) <- expEHITInfos
    assignment <- assignments
    (question, answer) <- assignment.response.qaPairs
    if !question.isEmpty
  } yield (hit.prompt.path, SourcedQAPair(hit.hitId, question, answer))

  // group the questions by which sentence they're asking about,
  // randomizing (in a repeatable way) the order of each sentence's QA pairs

  lazy val sourcedQAPairsBySentence = {
    val shuffleRand = new util.Random(555555555L)
    questionInfos.groupBy(_._1).map {
      case (path, items) => path -> shuffleRand.shuffle(items.map(_._2))
    }
  }

  // for each sentence, group its questions into lists of 10 each; then, randomize these sentence/group pairs
  val numQAsPerHIT = 6

  lazy val prompts = {
    val shuffleRand = new util.Random(444443333L)

    val inOrder = for {
      (path, qaPairs) <- sourcedQAPairsBySentence
      qaPairGroup <- qaPairs.grouped(numQAsPerHIT)
    } yield ValidationPrompt(path, qaPairGroup.toList)

    shuffleRand.shuffle(inOrder)
  }

  // NOTE for future tasks. Do more than 30 of each HIT Type at once.
  // If responding to individual assignments with review, this can work fine; in particular,
  // you can just put all of the HITs up at once. So that's a better way to do it.
  lazy val qvHelper = new HITManager.Helper(qvTaskSpec)
  lazy val qvHITManager = actorSystem.actorOf(Props(new NumAssignmentsHITManager(
    qvHelper,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    initNumHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)))
  lazy val qvActor = actorSystem.actorOf(Props(new TaskManager(qvHelper, qvHITManager)))

  lazy val avHelper = new HITManager.Helper(avTaskSpec)
  lazy val avHITManager = actorSystem.actorOf(Props(new NumAssignmentsHITManager(
    avHelper,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    initNumHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)))
  lazy val avActor = actorSystem.actorOf(Props(new TaskManager(avHelper, avHITManager)))

  // oh, ok, guess we should do longest-answer preferring validation too

  lazy val lavHelper = new HITManager.Helper(lavTaskSpec)
  lazy val lavHITManager = actorSystem.actorOf(Props(new NumAssignmentsHITManager(
    lavHelper,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    initNumHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)))
  lazy val lavActor = actorSystem.actorOf(Props(new TaskManager(lavHelper, lavHITManager)))

  import TaskManager._
  def start(interval: FiniteDuration = 1 minute) = {
    server
    qvActor ! Start(interval)
    avActor ! Start(interval)
    lavActor ! Start(interval)
  }
  def stop() = {
    qvActor ! Stop
    avActor ! Stop
    lavActor ! Stop
  }
  def disable() = {
    qvActor ! Disable
    avActor ! Disable
    lavActor ! Disable
  }
  def expire() = {
    qvActor ! Expire
    avActor ! Expire
    lavActor ! Expire
  }
  def update() = {
    server
    qvActor ! Update
    avActor ! Update
    lavActor ! Update
  }

  lazy val qData = FileManager.loadAllData[ValidationPrompt, QuestionValidationResponse](qvTaskSpec.hitTypeId)
  lazy val aData = FileManager.loadAllData[ValidationPrompt, AnswerValidationResponse](avTaskSpec.hitTypeId)
  lazy val laData = FileManager.loadAllData[ValidationPrompt, AnswerValidationResponse](lavTaskSpec.hitTypeId)

  case class ValidatedQAPair(
    sentence: CoNLLSentence,
    specialWord: Int,
    origQuestion: String,
    origAnswer: Set[Int],
    newQuestions: List[Option[String]],
    newShortAnswers: List[Option[Set[Int]]],
    newLongAnswers: List[Option[Set[Int]]],
    newShortAnswersTagged: List[(String, Option[Set[Int]])],
    newLongAnswersTagged: List[(String, Option[Set[Int]])]) {

    val allAnswers = newShortAnswers ++ newLongAnswers
    val sentenceString = TextRendering.renderSentence(sentence)
    val origAnswerString = expE.renderSpan(sentence, origAnswer)

    val numQuestionNAs = newQuestions.filter(_.isEmpty).size
    val numAnswerNAs = newShortAnswers.filter(_.isEmpty).size + newLongAnswers.filter(_.isEmpty).size

    val specialWordForms = inflections.getAllForms(TextRendering.normalizeToken(sentence.words(specialWord).token).lowerCase).map(_.toString)

    val origAnswerContainsSpecialWord = origAnswer.contains(specialWord)
    val origQuestionContainsSpecialWord = specialWordForms.exists(origQuestion.toLowerCase.contains)

    val newQuestionsContainingSpecialWord = newQuestions.flatten.filter(nq => specialWordForms.exists(nq.toLowerCase.contains))
    val longAnswersContainingSpecialWord = newLongAnswers.flatten.filter(na => na.contains(specialWord))
    val shortAnswersContainingSpecialWord = newShortAnswers.flatten.filter(na => na.contains(specialWord))
  }

  def printQuestions(vqa: ValidatedQAPair) = println(
    s"${vqa.sentenceString}\n(${vqa.sentence.words(vqa.specialWord).token}) ${vqa.origQuestion} --> ${vqa.origAnswerString}\n" +
      s"Invalid: ${vqa.numQuestionNAs}\n${vqa.newQuestions.flatten.mkString("\n")}\n"
  )
  def printAnswers(vqa: ValidatedQAPair) = println(
    s"${vqa.sentenceString}\n(${vqa.sentence.words(vqa.specialWord).token}) ${vqa.origQuestion} --> ${vqa.origAnswerString}\n" +
      s"Invalid: ${vqa.numAnswerNAs}\n${vqa.allAnswers.flatten.map(a => expE.renderSpan(vqa.sentence, a)).mkString("\n")}\n"
  )

  lazy val pathToHITToQAPairs: Map[CoNLLSentencePath, Map[String, List[ValidatedQAPair]]] =
    prompts.groupBy(_.path).map {
      case (path, pathPrompts) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        val hitToQAPairs = pathPrompts
          .flatMap(_.sourcedQAPairs)
          .groupBy(_.originalHITId).map {
          case (hitId, hitQAPairs) =>
            val specialWord = FileManager.getHIT[expE.QAGenPrompt](expE.qaGenFrozenHITTypeId, hitId).get.prompt.wordIndex
            val validatedQAPairs = hitQAPairs.map {
              case sqa @ SourcedQAPair(_, question, answer) =>
                val validationQuestions = qData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.questions(index))
                }.flatten
                val validationAnswers = aData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.answerIndices(index))
                }.flatten
                val validationAnswersTagged = aData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(a => (a.workerId, a.response.answerIndices(index)))
                }.flatten
                val longValidationAnswers = laData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.answerIndices(index))
                }.flatten
                val longValidationAnswersTagged = laData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(a => (a.workerId, a.response.answerIndices(index)))
                }.flatten
                ValidatedQAPair(sentence, specialWord, question, answer, validationQuestions.toList,
                                validationAnswers.toList, longValidationAnswers.toList,
                                validationAnswersTagged.toList, longValidationAnswersTagged.toList)
            }
            hitId -> validatedQAPairs.toList
        }.toMap
        path -> hitToQAPairs
    }.toMap

  lazy val allQAPairs = for {
    (path, hitToQAPairs) <- pathToHITToQAPairs.toList
    (_, vqas) <- hitToQAPairs.toList
    vqa <- vqas
  } yield vqa

  def makeExpETSV: String = {
    val sb = new StringBuilder
    pathToHITToQAPairs.foreach {
      case (path, hitToQAPairs) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        sb.append("\t\t\t" + TextRendering.renderSentence(sentence) + "\n")
        hitToQAPairs.foreach {
          case (hit, vQAPairs) =>
            vQAPairs.foreach {
              case ValidatedQAPair(_, specialWord, question, answerIndices, newQs, newAs, newLAs, _, _) =>
                val answer = expE.renderSpan(sentence, answerIndices)
                val longAnswerStrings = newLAs.map(_.fold("")(span => expE.renderSpan(sentence, span)))
                sb.append(s"${path.filePath.get}\t${path.sentenceNum}\t\t")
                sb.append(s"${TextRendering.normalizeToken(sentence.words(specialWord).token)} ($specialWord)\t$question\t")
                sb.append(expE.renderSpan(sentence, answerIndices) + s"\t${longAnswerStrings.mkString("\t")}\t")
                sb.append(s"${answerIndices.mkString(" ")}\t")
                sb.append(newLAs.map(_.fold("")(_.mkString(" "))).mkString("\t"))
                sb.append("\n")
            }
        }
        sb.append("\n")
    }
    sb.toString
  }

  def writeExpETSV = FileManager.saveDataFile(experimentName, "readable.tsv", makeExpETSV)

  def makeTSV: String = {
    val sb = new StringBuilder
    pathToHITToQAPairs.foreach {
      case (path, hitToQAPairs) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        sb.append(TextRendering.renderSentence(sentence) + "\n")
        hitToQAPairs.foreach {
          case (hit, vQAPairs) =>
            vQAPairs.foreach {
              case ValidatedQAPair(_, _, question, answerIndices, newQs, newAs, newLAs, _, _) =>
                val answer = expE.renderSpan(sentence, answerIndices)
                val renderedQs = newQs.map(_.fold("N/A")(q => q))
                val renderedAs = newAs.map(_.fold("N/A")(a => expE.renderSpan(sentence, a)))
                val renderedLAs = newLAs.map(_.fold("N/A")(a => expE.renderSpan(sentence, a)))
                val answers = (answer :: renderedLAs).zipAll("" :: renderedAs, "", "")
                val allRows = (question :: renderedQs).zipAll(answers, "", "")
                allRows.map {
                  case (q, (la, a)) => s"$q\t$la\t$a\n"
                }.foreach(sb.append)
                sb.append("\n")
            }
        }
        sb.append("\n")
    }
    sb.toString
  }

  def writeTSV = FileManager.saveDataFile(experimentName, "readable.tsv", makeTSV)

  lazy val avgAnswerLengthDiffs = for {
    (path, hitIdToQAPairs) <- pathToHITToQAPairs.iterator
    sentence = FileManager.getCoNLLSentence(path).get
    (_, validatedQAPairs) <- hitIdToQAPairs.toList
    ValidatedQAPair(sentence, _, origQuestion, origAnswer, newQuestions, newShortAnswers, newLongAnswers, _, _) <- validatedQAPairs
    lengthDiff <- for {
      longMean <- newLongAnswers.flatten.map(_.size).onlyIf(!_.isEmpty).map(_.mean)
      shortMean <- newShortAnswers.flatten.map(_.size).onlyIf(!_.isEmpty).map(_.mean)
    } yield longMean - shortMean
  } yield lengthDiff

  lazy val questionsUnchanged = for {
    (path, hitIdToQAPairs) <- pathToHITToQAPairs.toList
    (_, validatedQAPairs) <- hitIdToQAPairs.toList
    ValidatedQAPair(_, _, origQuestion, _, newQuestions, _, _, _, _) <- validatedQAPairs
  } yield newQuestions.flatten.filter(_.equals(origQuestion)).size

  def getWordsInQuestion(sentence: CoNLLSentence, string: String): Set[Int] = {
    val tokens = tokenize(string)
      .filterNot(reallyUninterestingTokens.contains)
    // NOTE: ideally we would also try decapitalizing; but, we didn't initially so this is consistent
    // .filterNot(t => stopwords.contains(TextRendering.normalizeToken(t).lowerCase)).toSet
    val moreTokens = tokens.map(t => TextRendering.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.words.filter(w => generalizedTokens.contains(w.token.lowerCase)).map(_.index).toSet
  }

  import DirectedHypergraphInduction._

  // qa pair extraction methods

  val origQAPairs = (sentence: CoNLLSentence, vqa: ValidatedQAPair) => {
    val origQTokens = getWordsInQuestion(sentence, vqa.origQuestion)
    if(!origQTokens.isEmpty) {
      AlignedQuestionAnswerPair(QuestionAnswerPair(vqa.origQuestion, vqa.origAnswer), origQTokens) :: Nil
    } else Nil
  }

  val sampledValidatedQAPairs = (sentence: CoNLLSentence, vqa: ValidatedQAPair) => {
    val result = for {
      q <- vqa.newQuestions.head
      a <- vqa.newLongAnswers.head
      qTokens = getWordsInQuestion(sentence, q)
    } yield AlignedQuestionAnswerPair(QuestionAnswerPair(q, a), qTokens)
    result.toList
  }

  val min2SamplesValidatedQAPairs = (sentence: CoNLLSentence, vqa: ValidatedQAPair) => {
    val result = for {
      q1 <- vqa.newQuestions(0)
      q2 <- vqa.newQuestions(1)
      a1 <- vqa.newLongAnswers(0)
      a2 <- vqa.newLongAnswers(1)
      q1Tokens = getWordsInQuestion(sentence, q1)
      q2Tokens = getWordsInQuestion(sentence, q2)
      allQTokens = q1Tokens.intersect(q2Tokens)
      allATokens = a1.intersect(a2)
      if !allQTokens.isEmpty && !allATokens.isEmpty
    } yield AlignedQuestionAnswerPair(QuestionAnswerPair(s"$q1 / $q2", allATokens), allQTokens)
    result.toList
  }

  import scalaz._
  import Scalaz._

  val majorityValidatedQAPairs = (sentence: CoNLLSentence, vqa: ValidatedQAPair) => {
    if(vqa.newQuestions.forall(!_.isEmpty) && vqa.newLongAnswers.forall(!_.isEmpty)) {
      val result = for {
        questions <- vqa.newQuestions.sequence
        answers <- vqa.newLongAnswers.sequence
        sampledQTokens = majorities(questions.map(getWordsInQuestion(sentence, _)))
        sampledATokens = majorities(answers)
        sampledQTokenWords = sampledQTokens.map(i => TextRendering.normalizeToken(sentence.words(i).token).toLowerCase)
        bestQuestion = questions.zip(
          questions.map(
            q => {
              val qTokens = q.split("\\s+").map(_.toLowerCase).toSet
              val symdiff = sampledQTokenWords -- qTokens ++ (qTokens -- sampledQTokenWords)
              symdiff.size
            })).minBy(_._2)._1
      } yield AlignedQuestionAnswerPair(QuestionAnswerPair(bestQuestion, sampledATokens), sampledQTokens)
      result.toList
    } else {
      List.empty[AlignedQuestionAnswerPair]
    }
  }

  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

  @memoize(maxSize = 5, expiresAfter = 1 hour)
  def pathToAnnotation(
    extractQAPairs: (CoNLLSentence, ValidatedQAPair) => List[AlignedQuestionAnswerPair]
  ): Map[CoNLLSentencePath, AnnotatedSentence] =
    pathToHITToQAPairs.iterator.toStream.map {
      case (path, hitIdToQAPairs) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        val qaPairs = for {
          (_, validatedQAPairs) <- hitIdToQAPairs.toList
          vqa <- validatedQAPairs
          qaPair <- extractQAPairs(sentence, vqa)
        } yield qaPair
        val annotation = AnnotatedSentence(path, qaPairs)
        path -> annotation
    }.toMap

  def getGoldAnnotations(
    extractQuestionIndices: (CoNLLSentence, String) => Set[Int]
  ) = qaGenExperiment.manualAnnotationsByPath.map {
    case (path, annos) =>
      val sentence = FileManager.getCoNLLSentence(path).get
      val alignedQAPairs = annos._3.map {
        case (question, answerIndices) =>
          AlignedQuestionAnswerPair(QuestionAnswerPair(question, answerIndices), extractQuestionIndices(sentence, question))
      }
      path -> AnnotatedSentence(path, alignedQAPairs)
  }

  val qaPairExtractors = List(
    "Original Turk" -> origQAPairs,
    "Sampled Validated" -> sampledValidatedQAPairs,
    "2-Sample Minimal Validated" -> min2SamplesValidatedQAPairs,
    "Majority Validated" -> majorityValidatedQAPairs)

  val allAnnotationSources: List[(String, Map[CoNLLSentencePath, AnnotatedSentence])] =
    "Personal" -> getGoldAnnotations(getWordsInQuestion) :: qaPairExtractors.map {
      case (k, v) => k -> pathToAnnotation(v)
    }

  // questions without special word

  def propQAsWithoutSpecialWord(
    extractQAPairs: (CoNLLSentence, ValidatedQAPair) => List[AlignedQuestionAnswerPair]
  ) = {
    lazy val result = for {
      (path, hitIdToQAPairs) <- pathToHITToQAPairs.toList
      sentence = FileManager.getCoNLLSentence(path).get
      (_, validatedQAPairs) <- hitIdToQAPairs.toList
      vqa <- validatedQAPairs
      alignedQAPair <- extractQAPairs(sentence, vqa)
    } yield (alignedQAPair.questionIndices union alignedQAPair.qaPair.answer).contains(vqa.specialWord)
    result.filter(identity).size.toDouble / result.size
  }

  // hypergraphs

  @memoize(maxSize = 5, expiresAfter = 1 hour)
  def pathToHypergraph(
    annotations: Map[CoNLLSentencePath, AnnotatedSentence]
  ): Map[CoNLLSentencePath, DirectedHypergraph[CoNLLWord, (List[String], Double)]] =
    annotations.map {
      case (path, annotation) =>
        path -> getHypergraph(annotation).mapNodes(annotation.sentence.words)
    }

  def hypergraphString(sentence: CoNLLSentence, hg: DirectedHypergraph[CoNLLWord, (List[String], Double)]) =
    hg.edges.toList.sortBy(-_.label._2).map {
      case Hyperedge((questions, maxCorr), qWord, aWords) =>
        val printableQWord = expE.printableWord(sentence, qWord.index)
        val answer = expE.renderSpan(sentence, aWords.map(_.index))
        f"$printableQWord%15s --> $answer%20s ($maxCorr%.4f)\n\t\t$questions"
    }.mkString("\n")

  def hypergraphStrings(annotations: Map[CoNLLSentencePath, DirectedHypergraph[CoNLLWord, (List[String], Double)]]) =
    annotations.map {
      case (path, hg) => hypergraphString(FileManager.getCoNLLSentence(path).get, hg)
    }

  lazy val allHypergraphStrings = allAnnotationSources.map {
    case (k, v) => k -> hypergraphStrings(pathToHypergraph(v))
  }

  // dependency trees

  // @memoize(maxSize = 5, expiresAfter = 1 hour)
  // def pathsAndTrees(
  //   annotations: Map[CoNLLSentencePath, AnnotatedSentence]
  // ): Stream[(CoNLLSentencePath, DependencyTree[TreeInduction.Index, Unit])] =
  //   annotations.toStream.map {
  //     case (path, annotation) =>
  //       val qaPairs = annotation.alignedQAPairs.map {
  //         case AlignedQuestionAnswerPair(QuestionAnswerPair(_, answer), questionIndices) =>
  //           (questionIndices, answer)
  //       }
  //       val sentence = FileManager.getCoNLLSentence(path).get
  //       val tree = TreeInduction.induceTree(sentence, qaPairs)
  //       (path -> tree)
  //   }

  // def printIndex(idx: TreeInduction.Index) = idx match {
  //   case TreeInduction.Root => "root"
  //   case TreeInduction.Word(w) => expE.printableWord(w)
  // }

  // def printTree(tree: DependencyTree[TreeInduction.Index, Unit]) =
  //   tree.toStringMultiline(_ => "", printIndex _)

  // def treeStrings(
  //   pathsAndTrees: Stream[(CoNLLSentencePath, DependencyTree[TreeInduction.Index, Unit])]
  // ) = pathsAndTrees.map {
  //   case (path, tree) => printTree(tree)
  // }

  // lazy val allTreeStrings = allAnnotationSources.map {
  //   case (k, v) => k -> treeStrings(pathsAndTrees(v))
  // }

  lazy val qaGenExperiment = new expE.QuestionWordExperiment

  import expG.ManualQARecord
  import expG.AlignedManualQARecord
  import expG.KeywordedQAPair
  import expG.AlignedKeywordedQAPair
  lazy val manualQA = new expG.ManualQA
  lazy val manualQARecordsByPath = manualQA.loadSavedData.map {
    case ManualQARecord(path, sentence, groups) =>
      AlignedManualQARecord(
        path, sentence,
        groups map { group =>
          group map {
            case KeywordedQAPair(question, keywordIndex, answer) =>
              AlignedKeywordedQAPair(question, keywordIndex, getWordsInQuestion(sentence, question), answer)
          }
        })
  }.groupBy(_.path).map { case (k, v) => k -> v.head }

  lazy val allQAPairAnalyses = allAnnotationSources.map {
    case (k, v) => k -> new QAPairAnalysis(
      goldAnnotations = manualQARecordsByPath,
      turkAnnotations = v,
      makeHypergraph = (as: AnnotatedSentence) => pathToHypergraph(v)(as.path).mapLabels(_._1))
  }

  lazy val qaPairAnalysisReport = allQAPairAnalyses.map {
    case (k, v) =>
      val specialWordProp = qaPairExtractors.find(_._1 == k)
        .map(_._2)
        .map(propQAsWithoutSpecialWord _)
        .fold("")(p => f"Proportion of QA pairs with special word: $p%.3f\n")
      s"=== $k ===\n$specialWordProp${v.allStatsString}"
  }.mkString("\n\n")

  lazy val shortIAA = new InterAnnotatorAgreement(
    allQAPairs.map(_.newShortAnswers),
    allQAPairs.map(_.newShortAnswersTagged))

  lazy val longIAA = new InterAnnotatorAgreement(
    allQAPairs.map(_.newLongAnswers),
    allQAPairs.map(_.newLongAnswersTagged))

  lazy val iaaReport = s"""
=== Long answer agreement ===
${longIAA.report}
=== Short answer agreement ===
${shortIAA.report}
"""


  lazy val completeReport = iaaReport + "\n==Gold==\n" + (allQAPairAnalyses.head._2.goldStatsString) + "\n" + qaPairAnalysisReport

  lazy val workerValidationInstances = for {
    HITInfo(hit, assignments) <- expEHITInfos
    assignment <- assignments
    (question, answer) <- assignment.response.qaPairs
    if !question.isEmpty
    validatedQAPair <- pathToHITToQAPairs(hit.prompt.path)(hit.hitId)
    if validatedQAPair.origQuestion.equals(question)
    numTotalAnswers = validatedQAPair.newShortAnswers.size + validatedQAPair.newLongAnswers.size
    numValidAnswers = validatedQAPair.newShortAnswers.flatten.size + validatedQAPair.newLongAnswers.flatten.size
  } yield (assignment.workerId, numValidAnswers, numTotalAnswers)

  val workerValidityRates = workerValidationInstances
    .groupBy(_._1)
    .map {
    case (workerId, instances) =>
      val totalValid = instances.map(_._2).sum
      val total = instances.map(_._3).sum
      workerId -> (totalValid.toDouble / total, total)
  }

  lazy val goodIndices = manualQARecordsByPath.values.zipWithIndex.filter(_._1.qaGroups.size > 1).map(_._2).toList
  def reportManualGraph(i: Int) = {
    val sentence = FileManager.getCoNLLSentence(manualQARecordsByPath.keys.toList(i)).get
    val qaPairs = manualQARecordsByPath.values.toList(i).qaGroups.flatten.toVector.map {
      kqa => AlignedQuestionAnswerPair(QuestionAnswerPair(kqa.question, kqa.answerIndices), kqa.questionIndices)
    }
    reportGraph(sentence, qaPairs)
  }

  def reportTurkGraph(path: CoNLLSentencePath) = {
    val sentence = FileManager.getCoNLLSentence(path).get
    val qaPairs = pathToAnnotation(origQAPairs)(path).alignedQAPairs.toVector
    reportGraph(sentence, qaPairs)
  }

  def reportGraph(sentence: CoNLLSentence, qaPairs: Vector[AlignedQuestionAnswerPair]) = {
    val answerSpans = qaPairs.map(
      kqa => kqa.qaPair.answer.filterNot(i => reallyUninterestingTokens.contains(sentence.words(i).token.toLowerCase))
    )
    val questionSpans = qaPairs.map(
      kqa => kqa.questionIndices.filterNot(i => reallyUninterestingTokens.contains(sentence.words(i).token.toLowerCase))
    )
    val qaArcs = qaPairs.map(
      kqa => (
        kqa.questionIndices.filterNot(i => reallyUninterestingTokens.contains(sentence.words(i).token.toLowerCase)),
        kqa.qaPair.answer.filterNot(i => reallyUninterestingTokens.contains(sentence.words(i).token.toLowerCase)))
    )
    val graph = GraphInduction.induceGraphSingleTreeSubroutines(questionSpans ++ answerSpans, qaArcs)
    println(
      graph.edges.toVector.map(
        e => s"${e.lesser}:${sentence.words(e.lesser).token} <--> ${e.greater}:${sentence.words(e.greater).token}"
      ).mkString("\n"))
  }

}
