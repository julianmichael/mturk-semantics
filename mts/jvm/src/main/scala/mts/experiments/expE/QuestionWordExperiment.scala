package mts.experiments.expE

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

class QuestionWordExperiment(implicit config: TaskConfig) {
  val experimentName = "questionWordQA"

  // for now, must agree with a string specified on the client as well. TODO refactor this
  val qaGenHITType = HITType(
    title = s"Write questions and answers about a word in context",
    description = s"""
      Given a sentence and a word from that sentence,
      write a question using the word whose answer is taken from the sentence.
      Come up with more question-answer pairs for a bonus!
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  lazy val qaGenApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(path, FileManager.getCoNLLSentence(path).get)
  }

  val sampleQAGenPrompt = QAGenPrompt(sentences.head._1, 0)

  lazy val qaGenTaskSpec = TaskSpecification[QAGenPrompt, QAGenResponse, ApiRequest, ApiResponse](
    TaskIndex.expEQAGenTaskKey, qaGenHITType, qaGenApiFlow, sampleQAGenPrompt)

  lazy val sourceSentences = for {
    ((path, sentence), sentenceIndex) <- sentences.zipWithIndex
    if sentenceIndex % 5 == 0 // only do 20 sentences
  } yield (path, sentence)

  lazy val sourcePrompts = {
    val inOrder = for {
      (path, sentence) <- sourceSentences
      i <- (0 until sentence.words.size)
      if !uninterestingTokens.contains(sentence.words(i).token)
    } yield QAGenPrompt(path, i)

    val shuffleRand = new util.Random(987654321L)

    shuffleRand.shuffle(inOrder.toVector)
  }

  lazy val qaGenHITManager = if(config.isProduction) {
    new QAGenHITManager(qaGenTaskSpec, 2, 60, sourcePrompts.iterator)
  } else {
    new QAGenHITManager(qaGenTaskSpec, 1, 4, sourcePrompts.iterator)
  }

  import config.actorSystem
  lazy val server = new Server(List(qaGenTaskSpec))
  lazy val qaGenActor = actorSystem.actorOf(Props(TaskManager(qaGenHITManager)))

  import qaGenHITManager.Message._
  def start(interval: FiniteDuration = 1 minute) = {
    server
    qaGenActor ! Start(interval)
  }
  def stop() = qaGenActor ! Stop
  def disable() = qaGenActor ! Disable
  def expire() = qaGenActor ! Expire
  def update() = {
    server
    qaGenActor ! Update
  }

  lazy val loadQAGenData = FileManager.loadAllData[QAGenPrompt, QAGenResponse](qaGenTaskSpec.hitTypeId)

  lazy val qaPairsBySentence = loadQAGenData.groupBy(_._1.prompt.path).toList.sortBy(_._1.toString).map {
    case (path, hits) =>
      val sentence = FileManager.getCoNLLSentence(path).get
      val qaPairs = hits.flatMap(_._2).flatMap(_.response.qaPairs)
      (sentence, qaPairs)
  }

  def getQAGenPromptInfo(prompt: QAGenPrompt) = {
    val sentence = FileManager.getCoNLLSentence(prompt.path).get
    val word = sentence.words(prompt.wordIndex)
    (sentence, word)
  }

  def getQAGenAssignmentInfo(sentence: CoNLLSentence, response: QAGenResponse) = {
    val qaPairs = response.qaPairs.map {
      case (question, answerIndexSet) =>
        val answer = {
          val answerTokens = sentence.words.filter(w => answerIndexSet.contains(w.index)).map(_.token)
          TextRendering.renderSentence(answerTokens)
        }
        (question, answer)
    }
    qaPairs
  }

  def getHITInfo(hitStuff: (HIT[QAGenPrompt], List[Assignment[QAGenResponse]])) = hitStuff match {
    case (HIT(_, _, prompt, _), assignments) =>
      val (sentence, word) = getQAGenPromptInfo(prompt)
      val assignmentInfos = assignments.map {
        case Assignment(_, _, _, _, _, _, response, feedback) =>
          val qaPairs = getQAGenAssignmentInfo(sentence, response)
          (qaPairs, feedback)
      }
      (sentence, word, assignmentInfos)
  }

  def loadAllInfo = loadQAGenData.map(getHITInfo)

  def allInfoString: String = {
    val sb = new java.lang.StringBuilder()
    val allInfo = loadQAGenData.groupBy(hitInfo => hitInfo._1.prompt.path)
    for((path, hits) <- allInfo) {
      val sentence = FileManager.getCoNLLSentence(path).get
      sb.append(TextRendering.renderSentence(sentence) + "\n")
      for {
        (hit, assignments) <- hits.sortBy(_._1.prompt.wordIndex)
        specialWord = sentence.words(hit.prompt.wordIndex).token
        assignment <- assignments
        qaPairs = assignment.response.qaPairs.filter(p => !p._1.isEmpty && !p._2.isEmpty)
        (question, answerIndices) <- qaPairs
      } yield {
        val answer = TextRendering.renderSentence(
          sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))
        sb.append(s"${assignment.workerId}\t${specialWord}\t${question}\t${answer}\n")
      }
    }
    sb.toString
  }

  def totalNumQAPairs = {
    val nums = for {
      (_, assignments) <- loadQAGenData
      assignment <- assignments
    } yield {
      val qaPairs = assignment.response.qaPairs.filter(p => !p._1.isEmpty && !p._2.isEmpty)
      qaPairs.size
    }
    nums.sum
  }

  def totalCost = {
    val costs = for {
      (_, assignments) <- loadQAGenData
      assignment <- assignments
    } yield {
      val qaPairs = assignment.response.qaPairs.filter(p => !p._1.isEmpty && !p._2.isEmpty)
      print(qaPairs.size + "\t")
      0.10 + bonuses.take(qaPairs.size).sum
    }
    costs.sum
  }

  def specialWordStats = {
    var specialWordAnswers = 0
    var specialWordVerbatimQuestions = 0
    var badQAPairs = List.empty[(String, String, CoNLLWord, String, String)]

    for {
      (hit, assignments) <- loadQAGenData
      assignment <- assignments
      (question, answerIndices) <- assignment.response.qaPairs
    } yield {
      if(answerIndices.contains(hit.prompt.wordIndex)) {
        specialWordAnswers = specialWordAnswers + 1
      } else {
        val sentence = FileManager.getCoNLLSentence(hit.prompt.path).get
        if(question.toLowerCase.contains(sentence.words(hit.prompt.wordIndex).token.toLowerCase)) {
          specialWordVerbatimQuestions = specialWordVerbatimQuestions + 1
        } else {
          if(!question.isEmpty && !answerIndices.isEmpty) {
            val answerString = TextRendering.renderSentence(
              sentence.words.filter(w => answerIndices.contains(w.index)).map(_.token))
            badQAPairs = (assignment.workerId, TextRendering.renderSentence(sentence), sentence.words(hit.prompt.wordIndex), question, answerString) :: badQAPairs
          }
        }
      }
    }
    (specialWordAnswers, specialWordVerbatimQuestions, badQAPairs)
  }

  def writeAssignmentStats = {
    val sb = new java.lang.StringBuilder()
    sb.append(
      "hitType\thitId\tassignmentId\tworkerId\tacceptTime\tsubmitTime\tworkerAssignmentNum\tnumQAPairs\n"
    )
    val assignments = loadQAGenData.map(_._2).flatten.sortBy(_.acceptTime)
    val assignmentNums = Counter[String]
    for {
      (hit, assignments) <- loadQAGenData
      // specialWord = sentence.words(hit.prompt.wordIndex).token
      assignment <- assignments
    } yield {
      import assignment._
      assignmentNums.add(workerId)
      val numQAPairs = assignment.response.qaPairs.filter(isQAPairNonempty).size
      sb.append(
        s"${hitTypeId}\t${hitId}\t${assignmentId}\t${workerId}\t${acceptTime}\t${submitTime}\t${assignmentNums.get(workerId)}\t${numQAPairs}\n"
      )
    }
    FileManager.saveDataFile(experimentName, "assignments.tsv", sb.toString)
  }

  lazy val manualAnnotations = {
    val lineList = FileManager.loadDataFile(experimentName, "manual.tsv").get
    def makeList(lineList: List[String]): List[(CoNLLSentencePath, CoNLLSentence, List[(String, Set[Int])])] = {
      if(lineList.isEmpty) {
        Nil
      } else {
        val (cur, remainder) = lineList.span(!_.trim.isEmpty)
        val (spaces, recursor) = remainder.span(_.trim.isEmpty)
        val (sentenceString, qaPairs) = (cur.head, cur.tail.map(_.trim.split("\t")))
        val (path, sentence) = sourceSentences
          .map(p => (p, longestCommonPrefix(TextRendering.renderSentence(p._2), sentenceString)))
          .maxBy(_._2)
          ._1
        val processedQAPairs = for {
          lineArr <- qaPairs
          (question, answer) = (lineArr(0), lineArr(1))
          if sentenceString.contains(answer)
          answerTokens = tokenize(answer)
          beginIndex <- (0 until sentence.words.size).find(
            i => answerTokens.zipWithIndex.forall {
              case (token, j) => token.equals(sentence.words(i + j).token)
            }
          )
        } yield (question, (beginIndex until (beginIndex + answerTokens.size)).toSet)

        (path, sentence, processedQAPairs) :: makeList(recursor)
      }
    }
    makeList(lineList).sortBy(_._1.toString)
  }

  lazy val manualAnnotationsByPath = manualAnnotations.groupBy(_._1).map { case (k, v) => (k, v.head) }

  @Lenses case class SpanInfo(
    exactMatch: List[String],
    almost: List[String],
    contained: List[String],
    missed: List[String])
  object SpanInfo {
    def zero = SpanInfo(Nil, Nil, Nil, Nil)
  }
  lazy val spanAgreement = {
    val allInfo = loadQAGenData.groupBy(hitInfo => hitInfo._1.prompt.path)
    val allSpanInfos = for((path, hitInfos) <- allInfo) yield {
      val (_, sentence, manualQAPairs) = manualAnnotationsByPath(path)
      val turkQAPairs = hitInfos.flatMap(_._2).flatMap(_.response.qaPairs)
      val turkAnswerSpans = turkQAPairs.map(_._2)
      import SpanInfo._
      val infos = manualQAPairs.map(_._2).toSet.toList.foldLeft(zero) {
        case (spanInfo, answerIndices) =>
          val answerSpan = TextRendering.renderSentence(sentence.words.filter(w => answerIndices(w.index)).map(_.token))
          def modify =
            if(turkAnswerSpans.contains(answerIndices)) exactMatch.modify _
            else if(turkAnswerSpans.map(as => (as -- answerIndices ++ (answerIndices -- as)).size).min < 2) almost.modify _
            else if(turkAnswerSpans.exists(answerIndices.subsetOf)) contained.modify _
            else missed.modify _
          modify(answerSpan :: _)(spanInfo)
      }
      (TextRendering.renderSentence(sentence), infos)
    }
    allSpanInfos
  }

  def printTotalSpanInfo = {
    def getStat(f: SpanInfo => List[String]) = spanAgreement.map(x => f(x._2).size).sum
    val exactMatch = getStat(_.exactMatch)
    val almost = getStat(_.almost)
    val contained = getStat(_.contained)
    val missed = getStat(_.missed)
    val total = exactMatch + almost + contained + missed
    def printStat(label: String, i: Int) = println(s"$label: $i (${percent(i, total)})")
    printStat("exactMatch", exactMatch)
    printStat("almost", almost)
    printStat("contained", contained)
    printStat("missed", missed)
  }

  // try to determine constituency
  // XXX this didn't work, and it used dependencies on molt and ordered,
  // so I'm commenting it out for now so we don't need the new dep

  // sealed trait ScoredOption[+A] {
  //   final def map[B](f: A => B): ScoredOption[B] = this match {
  //     case ScoredNone => ScoredNone
  //     case ScoredSome(a, aScore) => ScoredSome(f(a), aScore)
  //   }
  //   final def flatMap[B](f: A => ScoredOption[B]): ScoredOption[B] = this match {
  //     case ScoredNone => ScoredNone
  //     case ScoredSome(a, aScore) => f(a) match {
  //       case ScoredNone => ScoredNone
  //       case ScoredSome(b, bScore) => ScoredSome(b, aScore + bScore)
  //     }
  //   }
  //   final def toOption: Option[A] = this match {
  //     case ScoredNone => None
  //     case ScoredSome(a, _) => Some(a)
  //   }
  // }
  // case object ScoredNone extends ScoredOption[Nothing]
  // case class ScoredSome[+A](a: A, score: Double) extends ScoredOption[A]

  // val TreeSymbol = new molt.syntax.cnf.ParseSymbol[SyntaxTree]("Tree")
  // def getConstituencyTrees(sentence: CoNLLSentence, spans: List[Set[Int]]) = {
  //   import ordered._
  //   import molt.syntax.cnf._
  //   import SyncCNFProduction._

  //   val tokenCoNLLWords = sentence.words.map(w => write(w))

  //   val spanRanges = spans.map(span => (span.min, span.max))
  //   def treeRange(tree: SyntaxTree) = tree.fold(w => (w.index, w.index)) {
  //     case (_, childRanges) => (childRanges.head._1, childRanges.last._2)
  //   }

  //   // what we don't want is a span starting outside the tree and ending inside it (not including the ends)
  //   def rangesCross(r1: (Int, Int))(r2: (Int, Int)): Boolean =
  //     (r1._1 < r2._1 && r1._2 > r2._1 && r1._2 < r2._2) || (r1._1 > r2._1 && r1._1 < r2._2 && r1._2 > r2._2)

  //   def crossingPenalty(range: (Int, Int)) = {
  //     spanRanges.filter(rangesCross(range)).size.toDouble
  //   }
  //   // epsilon to prefer right-branching
  //   val epsilon = 0.01

  //   implicit object ScoredOptionInstance extends MonoidalCategoryOfCommutativeSemigroups[ScoredOption] {
  //     override def empty[A] = ScoredNone
  //     // not actually commutative (if scores are same)
  //     override def plus[A](s1: ScoredOption[A], s2: ScoredOption[A]) = (s1, s2) match {
  //       case (ScoredNone, x) => x
  //       case (x, ScoredNone) => x
  //       case (ScoredSome(_, score1), ScoredSome(_, score2)) => if(score1 <= score2) s1 else s2
  //     }
  //     override def times[A, B](sOpt1: ScoredOption[A], sOpt2: ScoredOption[B]) = for {
  //       s1 <- sOpt1
  //       s2 <- sOpt2
  //     } yield (s1, s2)
  //   }

  //   val lex = CNFLexical[ScoredOption, SyntaxTree](
  //     TreeSymbol, token => ScoredSome[SyntaxTree](SyntaxTreeLeaf(read[CoNLLWord](token)), 0.0))
  //   val combine = CNFBinary[ScoredOption, SyntaxTree, SyntaxTree, SyntaxTree](
  //     TreeSymbol, TreeSymbol, TreeSymbol, (children: ScoredOption[(SyntaxTree, SyntaxTree)]) => children.flatMap {
  //       case (left, right) =>
  //         val result = SyntaxTreeNode("X", List(left, right))
  //         val rightBranchingPenalty = if(left.depth > right.depth) epsilon else 0.0
  //         ScoredSome(result, crossingPenalty(treeRange(result)) + rightBranchingPenalty)
  //     })
  //   val parser = new GeneralizedCKYParser(lex :: combine :: Nil)
  //   parser.parse(tokenCoNLLWords.toVector, TreeSymbol)
  // }

  // lazy val turkSentenceAnswerSpans = for {
  //   (path, hitInfos) <- loadQAGenData.groupBy(_._1.prompt.path)
  //   sentence = FileManager.getCoNLLSentence(path).get
  // } yield (sentence, hitInfos.flatMap(_._2).flatMap(_.response.qaPairs).map(_._2).filterNot(_.isEmpty))

  // lazy val manualSentenceAnswerSpans = for {
  //   (_, sentence, qaPairs) <- manualAnnotations
  // } yield (sentence, qaPairs.map(_._2))

  // def saveTurkTrees = {
  //   val trees = turkSentenceAnswerSpans
  //     .map(Function.tupled((x: CoNLLSentence, y: List[Set[Int]]) => getConstituencyTrees(x,y)))
  //     .map { case ScoredSome(a, _) => a }
  //   FileManager.saveDataFile(experimentName, "turkTrees", trees.map(_.toStringMultiline).mkString("\n"))
  // }
  // def saveManualTrees = {
  //   val trees = manualSentenceAnswerSpans
  //     .map(Function.tupled((x: CoNLLSentence, y: List[Set[Int]]) => getConstituencyTrees(x,y)))
  //     .map { case ScoredSome(a, _) => a }
  //   FileManager.saveDataFile(experimentName, "manualTrees", trees.map(_.toStringMultiline).mkString("\n"))
  // }

  // compute mutual information between words in questions and answers

  def getWordsInQuestion(sentence: CoNLLSentence, string: String): Set[Int] = {
    val tokens = tokenize(string)
      .filterNot(reallyUninterestingTokens.contains)
    // NOTE: ideally we would also try decapitalizing; but, we didn't initially so this is consistent
    // .filterNot(t => stopwords.contains(TextRendering.normalizeToken(t).lowerCase)).toSet
    val moreTokens = tokens.map(t => TextRendering.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.words.filter(w => generalizedTokens.contains(w.token.lowerCase)).map(_.index).toSet
  }
  def getMutualInformationScores(sentence: CoNLLSentence, qaPairs: List[(String, Set[Int])]) = {
    import scala.collection.mutable
    val cooccurrence = mutable.Map.empty[(Int, Int), List[(String, (Set[Int]))]].withDefaultValue(Nil)
    val qCounts = Counter[Int]
    val aCounts = Counter[Int]
    val scores = for {
      (question, answerIndices) <- qaPairs
      qi <- getWordsInQuestion(sentence, question)
      ai <- answerIndices.filterNot(reallyUninterestingTokens.contains)
    } yield {
      cooccurrence.put((qi, ai), (question, answerIndices) :: cooccurrence((qi, ai)))
      qCounts.add(qi)
      aCounts.add(ai)
    }
    val total = cooccurrence.values.map(_.size).sum
    val totalQ = qCounts.sum
    val totalA = aCounts.sum

    val mutualInformations = cooccurrence.iterator.map {
      case ((qi, ai), questions) =>
        val prob = questions.size.toDouble / total
        val qProb = qCounts(qi).toDouble / totalQ
        val aProb = aCounts(ai).toDouble / totalA
        val mi = prob * math.log(prob / (qProb * aProb))
        (qi, ai, mi, questions)
    }.toList
    mutualInformations
  }

  def getFormattedMutualInformationScores(data: Iterable[(CoNLLSentence, List[(String, Set[Int])])]) = for {
    (sentence, qaPairs) <- data
    scores = getMutualInformationScores(sentence, qaPairs)
    processedScores = scores.map {
      case (qi, ai, mi, qaPairs) =>
        val questions = qaPairs.map(_._1)
        val answerSpans = qaPairs.map(_._2)
        // keep words that appear in more than half of the answers
        val protoAnswerSpan = answerSpans.flatten.toSet
          .filter(ai => answerSpans.filter(_.contains(ai)).size >= (answerSpans.size / 2))
        (qi, ai, mi, protoAnswerSpan, questions)
    }
    groupedScores = for {
      ((qi, protoAnswerSpan), entries) <- processedScores.groupBy(t => (t._1, t._4)).toVector
      answerWords = (entries.map(_._2), entries.map(_._3)).zipped.map {
        case (ai, mi) => (printableWord(sentence, ai), mi)
      }.sortBy(-_._2)
      averageMI = entries.map(_._3).sum / entries.size
      allQuestions = entries.map(_._5).flatten.toSet.toList
    } yield (printableWord(sentence, qi), answerWords, averageMI, renderSpan(sentence, protoAnswerSpan), allQuestions)
    sortedPrintableScores = groupedScores.sortBy(-_._3)
  } yield (sentence, sortedPrintableScores)

  lazy val manualMutualInformationScores =
    getFormattedMutualInformationScores(manualAnnotations.map(p => (p._2, p._3)))

  lazy val turkMutualInformationScores =
    getFormattedMutualInformationScores(qaPairsBySentence)

  def miInfoString(t: Vector[(String, List[(String, Double)], Double, String, List[String])]) = {
    val sb = new StringBuilder
    t foreach {
      case (q, a, s, protoAnswer, qList) =>
        val answerWords = a.map { case (w, mi) => f"$w%s($mi%.4f)"}.mkString(", ")
        val depString = s"$q --> $protoAnswer"
        val questions = qList.mkString("|")
        sb.append(f"$q%-10s --> $protoAnswer%-10s\n\t\t$s%.4f\t$answerWords%s\n\t\t$questions%s\n")
    }
    sb.toString
  }

  def predArgStructureString(path: CoNLLSentencePath): String = {
    val sentence = FileManager.getCoNLLSentence(path).get
    val depStrings = for {
      PredicateArgumentStructure(pred, args) <- sentence.predicateArgumentStructures
      head = printableWord(pred.head)
      ArgumentSpan(label, words) <- args
      if !label.equals("V")
      argPhrase = TextRendering.renderSentence(words.map(_.token))
    } yield f"$head%-20s --> $argPhrase%-20s ($label)"
    TextRendering.renderSentence(sentence) + "\n" + depStrings.mkString("\n")
  }

  def saveManualMIs = FileManager.saveDataFile(
    experimentName, "manualMIs.txt", manualMutualInformationScores.map {
      case (sentence, infos) => TextRendering.renderSentence(sentence) + "\n" + miInfoString(infos)
    }.mkString("\n\n"))

  def saveTurkMIs = FileManager.saveDataFile(
    experimentName, "turkMIs.txt", turkMutualInformationScores.map {
      case (sentence, infos) => TextRendering.renderSentence(sentence) + "\n" + miInfoString(infos)
    }.mkString("\n\n"))


  def saveOntonotesPredArgStructures = FileManager.saveDataFile(
    experimentName, "predArgs.txt", sourceSentences.map(_._1).sortBy(_.toString).map(predArgStructureString).mkString("\n\n"))

  sealed trait Index {
    def printable = this match {
      case Root => "root"
      case Word(w) => printableWord(w)
    }
  }
  case object Root extends Index
  case class Word(word: CoNLLWord) extends Index

  def induceTree(sentence: CoNLLSentence, qaPairs: List[(String, Set[Int])]): DependencyTree[Index, Unit] = {
    import gurobi._
    val env: GRBEnv = new GRBEnv("ilp-deps.log")
    val model: GRBModel = new GRBModel(env)
    model.set(GRB.IntParam.LogToConsole, 0)

    try {
      // Construct the ARBORESCENCE POLYTOPE!!!!!

      val allIndices: List[Index] = Root :: sentence.words.map(Word(_))
      val n = sentence.words.size.toDouble

      case class Arc(parent: Index, child: Index) {
        private[this] def shortIndex(i: Index) = i match {
          case Root => "$"
          case Word(w) => w.index
        }
        // not sure if variable names have to be unique...?
        override def toString: String = s"${shortIndex(parent)}-${shortIndex(child)}"
      }

      def incoming(index: Index): List[Arc] = allIndices.map(Arc(_, index))
      def outgoing(index: Index): List[Arc] = allIndices.map(Arc(index, _))

      sealed trait LPVar
      case class ArcVar(arc: Arc) extends LPVar {
        override def toString = s"a$arc"
      }
      case class FlowVar(arc: Arc) extends LPVar {
        override def toString = s"f$arc"
      }
      case class DisjunctiveVar(arcs: List[Arc]) extends LPVar {
        override def toString = s"d${arcs.mkString(",")}".hashCode.toString.take(255)
      }

      // Create variables
      val vars = collection.mutable.Map.empty[LPVar, GRBVar]
      for {
        parent <- allIndices
        child <- allIndices
      } yield {
        val arc = Arc(parent, child)

        val arcVar = ArcVar(arc)
        // arc is binary --- TODO make this real valued if we do relaxation
        val arcGRBVar = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, arcVar.toString)
        vars.put(arcVar, arcGRBVar)

        val flowVar = FlowVar(arc)
        // flow ranges from 0 to n
        val flowGRBVar = model.addVar(0.0, n, 0.0, GRB.CONTINUOUS, flowVar.toString)
        vars.put(flowVar, flowGRBVar)
      }
      model.update()

      // Add constraints

      // CONSTRAINT SET 1 (n): each node except root has exactly one parent
      for {
        child <- allIndices
        if child != Root
      } yield {
        val expr = new GRBLinExpr()
        for(arc <- incoming(child)) {
          expr.addTerm(1.0, vars(ArcVar(arc)))
        }
        model.addConstr(expr, GRB.EQUAL, 1.0, s"$child-parents")
      }

      // CONSTRAINT SET 2 (1): root has no parents
      val _ = {
        val expr = new GRBLinExpr()
        for(arc <- incoming(Root)) {
          expr.addTerm(1.0, vars(ArcVar(arc)))
        }
        model.addConstr(expr, GRB.EQUAL, 0.0, "root-noparents")
      }

      // CONSTRAINT SET 3 (1): root sends flow n
      val __ = {
        val expr = new GRBLinExpr()
        for(arc <- outgoing(Root)) {
          expr.addTerm(1.0, vars(FlowVar(arc)))
        }
        model.addConstr(expr, GRB.EQUAL, n, "root-flow-n")
      }

      // CONSTRAINT SET 4 (n): Each non-root node consumes 1 flow
      for {
        node <- allIndices
        if node != Root
      } yield {
        val expr = new GRBLinExpr()
        // add inward flow
        for(arc <- incoming(node)) {
          expr.addTerm(1.0, vars(FlowVar(arc)))
        }
        // subtract outward flow
        for(arc <- outgoing(node)) {
          expr.addTerm(-1.0, vars(FlowVar(arc)))
        }
        // result == 1
        model.addConstr(expr, GRB.EQUAL, 1.0, s"$node-flow-consume")
      }

      // CONSTRAINT SET 5 (n^2): flow is 0 on disabled arcs
      for {
        parent <- allIndices
        child <- allIndices
      } yield {
        val arc = Arc(parent, child)

        val left = new GRBLinExpr()
        left.addTerm(1.0, vars(FlowVar(arc)))
        val right = new GRBLinExpr()
        right.addTerm(n, vars(ArcVar(arc)))
        model.addConstr(left, GRB.LESS_EQUAL, right, s"$parent-$child-flow")
      }

      // done with constraints; we're now in the arborescence polytope.
      // QA pairs determine the objective.
      val qaPairIndices = qaPairs.map {
        case (question, answerIndices) =>
          (getWordsInQuestion(sentence, question).map(sentence.words(_)), answerIndices.map(sentence.words(_)))
      }

      val arcScores = Scorer[Arc, Double]
      for((qis, ais) <- qaPairIndices) {
        // ask for question words to be connected
        for(p <- qis; c <- qis) {
          if(p != c) arcScores.add(Arc(Word(p), Word(c)), 1.0)
        }
        // ask for answer words to be connected
        for(p <- ais; c <- ais) {
          if(p != c) arcScores.add(Arc(Word(p), Word(c)), 1.0)
        }

        // ask for question words to be connected to answer words (weighted down)
        val qToAScore = 1.0 / (qis.size * ais.size)
        // val aToQScore = 0.2 / (qis.size * ais.size)
        for(q <- qis; a <- ais) {
          if(q != a) {
            arcScores.add(Arc(Word(q), Word(a)), qToAScore)
            // arcScores.add(Arc(Word(a), Word(q)), aToQScore)
          }
        }
      }

      val objective = new GRBLinExpr()

      // // ask for question words to be connected to answer words (disjunctive)
      // val qaPairConnectionWeight = 0.5
      // for((qis, ais) <- qaPairIndices) {
      //   val qaArcs = (for(p <- qis; c <- ais) yield Arc(Word(p), Word(c))).toList
      //   val disjVar = DisjunctiveVar(qaArcs)
      //   val disjGRBVar = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, disjVar.toString)
      //   vars.put(disjVar, disjGRBVar)
      //   for(arc <- qaArcs) {
      //     val left = new GRBLinExpr()
      //     left.addTerm(1.0, disjGRBVar)
      //     val right = new GRBLinExpr()
      //     right.addTerm(1.0, vars(ArcVar(arc)))
      //     model.addConstr(left, GRB.GREATER_EQUAL, right, s"$disjVar-greater-$arc")
      //   }
      //   val left = new GRBLinExpr()
      //   left.addTerm(1.0, disjGRBVar)
      //   val right = new GRBLinExpr()
      //   for(arc <- qaArcs) {
      //     right.addTerm(1.0, vars(ArcVar(arc)))
      //   }
      //   model.addConstr(left, GRB.LESS_EQUAL, right, s"$disjVar-less")
      //   objective.addTerm(qaPairConnectionWeight, disjGRBVar)
      //   model.update()
      // }

      for((arc, weight) <- arcScores.iterator) {
        objective.addTerm(weight, vars(ArcVar(arc)))
      }
      model.setObjective(objective, GRB.MAXIMIZE)

      // run the optimization
      model.optimize()

      // TODO construct dependency tree from the results
      val arcs = for {
        pIndex <- allIndices
        cIndex <- allIndices
        arc = Arc(pIndex, cIndex)
        if vars(ArcVar(arc)).get(GRB.DoubleAttr.X) > 0.9 // TODO proper way to get binary value?
      } yield arc

      // since there are no cycles, we can safely do this
      def treeAt(index: Index): DependencyTree[Index, Unit] = {
        val childArcs = arcs
          .filter(_.parent == index)
          .map(arc => ((), treeAt(arc.child)))
        DependencyTree[Index, Unit](index, childArcs)
      }

      val tree = treeAt(Root)

      tree
    } catch {
      case e: Exception => e.printStackTrace
        null // TODO meh option return type
    } finally {
      model.dispose()
      env.dispose()
    }
  }

  lazy val manualTrees = manualAnnotations.toStream.map(p => induceTree(p._2, p._3))
  lazy val turkTrees = qaPairsBySentence.toStream.map(Function.tupled(induceTree))

  def printTree(t: DependencyTree[Index, Unit]) = t.toStringMultiline(_ => "", _.printable)

}
