package turksem.qasrl

import qasrl._
import qasrl.crowd._
import qasrl.util._

import cats.Id
import cats.implicits._
import turksem.util._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

object QALabelMapper {

  val coreArgCollapsedLabels = Set(
    "subj-transitive", "subj-intransitive", "obj", "obj-dative"
  ).map(_.lowerCase)

  val advCollapsedLabels = Set(
    "when", "where", "why", "how", "how much", "how long"
  ).map(_.lowerCase)

  def getQuestionsForCollapsedLabels(
    verbInflectedForms: InflectedForms,
    collapsedLabels: List[LowerCaseString]
  ): List[LowerCaseString] = {
    val givenLabelSet = collapsedLabels.toSet
    val coreNounArgs = coreArgCollapsedLabels.filter(givenLabelSet.contains)
    val advArgs = advCollapsedLabels.filter(givenLabelSet.contains)
    val prepArgs = givenLabelSet -- coreNounArgs -- advArgs

    val initFrame = Frame(
      verbInflectedForms,
      DependentMap.empty[ArgumentSlot.Aux, Id],
      PastTense,
      false, false, false, false)

    collapsedLabels.map(_.toString).map { label =>
      label match {
        case "subj-intransitive" =>
          val theseArgs = List(
            Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
            prepArgs.headOption.map(prep =>
              DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
            )
          ).flatten
          initFrame.copy(
            args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
          ).questionsForSlot(Subj).head
        case "subj-transitive" | "obj" =>
          val argSlot = if(label == "subj-transitive") Subj else Obj
          val theseArgs = List(
            Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
            Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
            prepArgs.headOption.map(prep =>
              DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
            )
          ).flatten
          initFrame.copy(
            args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
          ).questionsForSlot(argSlot).head
        case "obj-dative" =>
          val theseArgs = List(
            Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
            Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
            Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Noun(false)))
          ).flatten
          initFrame.copy(
            args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
          ).questionsForSlot(Obj).head
        case adv if advCollapsedLabels.contains(adv.lowerCase) =>
          val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
            List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
              prepArgs.headOption.map(prep =>
                DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
              )
            ).flatten
          } else {
            List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              prepArgs.headOption.map(prep =>
                DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
              )
            ).flatten
          }

          val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
            !givenLabelSet.contains("subj-intransitive".lowerCase) &&
            !givenLabelSet.contains("subj-transitive".lowerCase)

          initFrame.copy(
            args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
            isPassive = newFrameIsPassive
          ).questionsForSlot(Adv(adv.lowerCase)).head
        case prep => // all others are prepositions
          val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
            List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
            ).flatten
          } else {
            List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
            ).flatten
          }

          val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
            !givenLabelSet.contains("subj-intransitive".lowerCase)
          initFrame.copy(
            args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
            isPassive = newFrameIsPassive
          ).questionsForSlot(Obj2).head
      }
    }.map(_.lowerCase)
  }

  val mainAuxVerbs = {
    val negContractibleAuxes = Set(
      "has", "had",
      "might", "would", "should",
      "does", "did",
      "is", "was"
    )
    val allAuxes = negContractibleAuxes ++
      negContractibleAuxes.map(_ + "n't") ++
      Set(
        "can", "will",
        "can't", "won't"
      )
    allAuxes.map(_.lowerCase)
  }

  def getVerbAbstractedSlotLabel(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Option[LowerCaseString]] = {
    val labelOpts = getExplicitTemplateLabelsForQuestion(sentenceTokens, verbInflectedForms, questions)
    labelOpts.map { labelOpt =>
      for {
        slotsStr <- labelOpt
        vec = slotsStr.split("\t").toVector
        mainVerb = vec(3).split(" ").last
        mainVerbForm <- verbInflectedForms.getForm(mainVerb.lowerCase)
      } yield vec.updated(3, (vec(3).split(" ").init ++ Seq(mainVerbForm)).mkString(" ")).mkString("\t").lowerCase
    }
  }

  def getCheckedLabelsForQuestion(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Option[LowerCaseString]] = {
    val results = getCollapsedLabels(sentenceTokens, verbInflectedForms, questions)
    val reQuestions = getQuestionsForCollapsedLabels(verbInflectedForms, results.map(_.get))
    questions.zip(reQuestions).foreach { case (question, reQuestion) =>
      println
      println(s"Original: $question")
      println(s"New:      $reQuestion")
      println
    }
    results
  }

  def getExplicitTemplateLabelsForQuestion(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Option[LowerCaseString]] = questions.map { question =>
    val stateMachine = new TemplateStateMachine(sentenceTokens, verbInflectedForms)
    val template = new QuestionProcessor(stateMachine)
    template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => Option.empty[LowerCaseString]
      case Right(goodStates) => goodStates.toList.collect {
        case QuestionProcessor.CompleteState(_, frame, answerSlot) =>
          val wh = answerSlot match {
            case Subj => if(frame.args.get(Subj).get.isAnimate) "who" else "what"
            case Obj => if(frame.args.get(Obj).get.isAnimate) "who" else "what"
            case Obj2 => frame.args.get(Obj2).get match {
              case Noun(isAnimate) => if(isAnimate) "who" else "what"
              case Prep(_, Some(Noun(isAnimate))) => if(isAnimate) "who" else "what"
              case Locative => "where"
              case _ => "what" // extra case for objless prep; shouldn't happen
            }
            case Adv(wh) => wh.toString
          }
          val subj = {
            if(answerSlot == Subj) "_"
            else frame.args.get(Subj).fold("_"){ case Noun(isAnimate) =>
              if(isAnimate) "someone" else "something"
            }
          }
          val verbStack = if(subj == "_") frame.getVerbStack else frame.splitVerbStackIfNecessary(frame.getVerbStack)
          val (aux, verbForm) = if(subj != "_" || (verbStack.size > 1 && mainAuxVerbs.contains(verbStack.head.lowerCase))) {
            verbStack.head -> verbStack.tail.mkString(" ")
          } else {
            "_" -> verbStack.toList.mkString(" ")
          }

          val obj = {
            if(answerSlot == Obj) "_"
            else frame.args.get(Obj).fold("_"){ case Noun(isAnimate) =>
              if(isAnimate) "someone" else "something"
            }
          }

          val (prep, obj2) = {
            frame.args.get(Obj2).fold("_" -> "_") { arg =>
              if(answerSlot == Obj2) arg match {
                case Noun(isAnimate) => "_" -> "_"
                case Prep(preposition, _) =>
                  val vec = preposition.split(" ").toVector
                  if(vec.last == "do" || vec.last == "doing") {
                    vec.init.mkString(" ") -> vec.last
                  } else preposition.toString -> "_"
                case Locative => "_" -> "_"
              } else arg match {
                case Noun(isAnimate) => "_" -> (if(isAnimate) "someone" else "something")
                case Prep(preposition, Some(Noun(isAnimate))) =>
                  val vec = preposition.split(" ").toVector.init
                  if(vec.size > 0 && (vec.last == "do" || vec.last == "doing")) {
                    vec.init.mkString(" ") -> (vec.last + " something")
                  } else preposition.toString -> (if(isAnimate) "someone" else "something")
                case Prep(preposition, None) =>
                  preposition.toString -> "_"
                case Locative => "_" -> "somewhere"
              }
            }
          }
          val slots = List(wh, aux, subj, verbForm, obj, prep, obj2)
          // TODO factor this check out to a unit test
          val resultLCS = (slots.filter(_ != "_").mkString(" ") + "?").lowerCase
          val frameLCStrings = frame.questionsForSlot(answerSlot).map(_.lowerCase)
          val resultSansNeg = resultLCS.replaceAll("not", "").replaceAll("n't", "").replaceAll("\\s+", " ").lowerCase
          val origQuestionSansNeg = question.replaceAll("not", "").replaceAll("n't", "").replaceAll("\\s+", " ").lowerCase
          if(!frameLCStrings.contains(resultLCS) || resultSansNeg != origQuestionSansNeg) {
            println("Question results disagree!")
            println(s"Frame: $frame")
            println(s"Slots: " + slots.mkString("\t").lowerCase)
            println(s"Original question: ${question.lowerCase}")
            println(s"Frame strings: ${frameLCStrings.mkString(",")}")
            println(s"Slots string: $resultLCS")
          }
          slots.mkString("\t").lowerCase
      }.toSet.headOption
    }
  }

  // should always return a result for a properly QA-SRL formatted question
  def getAllCollapsedLabels(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Set[LowerCaseString]] = questions.map { question =>
    val stateMachine = new TemplateStateMachine(sentenceTokens, verbInflectedForms)
    val template = new QuestionProcessor(stateMachine)
    template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => Set.empty[LowerCaseString]
      case Right(goodStates) =>
        val framesWithAnswerSlots = goodStates.toList.collect {
          case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
        }.toSet
        val labels = framesWithAnswerSlots.toList.map {
          case (frame, Adv(whWord)) => whWord
          case (frame, Obj2) => frame.args.get(Obj2).get match {
            case Prep(preposition, _) =>
              if(frame.isPassive && preposition == "by".lowerCase) "subj-transitive".lowerCase
              else preposition
            case Noun(isAnimate) => (if(isAnimate) "obj-dative" else "obj").lowerCase // obj/2 ambiguous
            case Locative => "where".lowerCase
          }
          case (frame, Obj) => frame.args.get(Obj).get match {
            case Noun(isAnimate) =>
              if(frame.args.get(Obj2).nonEmptyAnd(_.isNoun) || frame.isPassive) { // obj/2 ambiguous
                (if(isAnimate) "obj-dative" else "obj").lowerCase
              } else "obj".lowerCase
          }
          case (frame, Subj) => frame.args.get(Subj).get match {
            case Noun(isAnimate) =>
              if(frame.isPassive) { // obj/2 ambiguous
                (if(frame.args.get(Obj).nonEmpty) "obj-dative" else "obj").lowerCase
              } else if(frame.args.get(Obj).nonEmpty) {
                "subj-transitive".lowerCase
              } else "subj-intransitive".lowerCase
          }
        }
        val res = labels.toSet.flatMap((label: LowerCaseString) =>
          if(stateMachine.prepositionBigrams.contains(label)) {
            label.split(" ").map(_.lowerCase).toSet
          } else Set(label)
        )
        res
    }
  }

  def useQuestionString(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Option[LowerCaseString]] = questions.map(question => Some(question.lowerCase))

  // should always return a result for a properly QA-SRL formatted question
  def getCollapsedLabels(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    questions: List[String]
  ): List[Option[LowerCaseString]] = {
    val labelSets = getAllCollapsedLabels(sentenceTokens, verbInflectedForms, questions)
    labelSets.zipWithIndex.map { case (labels, index) =>
      // prefer the prep as the label when ambiguous between prep and object, at least in these caes
      val objs = Set("obj", "obj-dative").map(_.lowerCase)
      val res = if(labels.size == 2 && labels.exists(objs.contains) && labels.exists(x => !objs.contains(x))) {
        labels -- objs
      } else {
        if(labels.isEmpty) {
          println
          println(s"Uh oh, discrete labeling was empty!")
          println(s"Question: ${questions(index)}")
          println
        } else if(labels.size > 1){
          // println(question)
          // println("\t" + framesWithAnswerSlots.mkString("\n\t"))
          // println("\t" + labels.toList.sortBy(_.toString).mkString("; "))
        }
        labels
      }
      res.headOption
    }
  }
}
