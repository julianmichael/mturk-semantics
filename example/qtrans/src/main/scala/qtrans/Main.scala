package qtrans

import qasrl.labeling.DiscreteLabel
import qasrl.labeling.SlotBasedLabel

// import turksem.qasrl.QALabelMapper

import cats.implicits._

import com.monovore.decline._

import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
import nlpdata.util.LowerCaseStrings._

object Main extends CommandApp(
  name = "reconstruct-questions",
  header = "Reconstructs QA-SRL questions from collapsed labels sent to stdin",
  main = {
    val verbTokenFileOpt = Opts.option[String](
      "tokens", help = "File containing all tokens (newline-separated) that you may wish to inflect.")
    val wiktionaryFileOpt = Opts.option[String](
      "wiktionary", help = "Wiktionary data directory.")
    val slotsOpt = Opts.flag(
      "slots", help = "Output questions in slot-formatted form"
    ).orFalse

    (verbTokenFileOpt |@| wiktionaryFileOpt |@| slotsOpt).map { (tokenFile, wiktionaryFile, useSlots) =>
      import java.nio.file.{Files, Paths}
      import scala.collection.JavaConverters._
      val tokens = Files.lines(Paths.get(tokenFile)).iterator.asScala.toSet
      val wiktionary = new WiktionaryFileSystemService(Paths.get(wiktionaryFile))
      val inflections = wiktionary.getInflectionsForTokens(tokens.iterator)
      val scanner = new java.util.Scanner(System.in)
      while(scanner.hasNextLine) {
        val line = scanner.nextLine
        val lineArr = line.split("###").toList
        def getField = lineArr.lift
        val resEith = for {
          sentenceTokens <- getField(0).toRight(
            s"Input line must begin with the sentence tokens followed by ###. Line: $line"
          ).map(_.split(" ").toVector)
          verbStr <- getField(1).toRight(
            s"Second entry of input line after ### must be the verb. Line: $line"
          )
          verbInflectedForms <- inflections.getInflectedForms(verbStr.lowerCase).toRight(
            s"Could not get inflections for verb $verbStr."
          )
          argLabelString <- getField(2).toRight(
            s"Third entry of input line must be the semicolon-separated list of arg labels. Line: $line"
          )
          discreteArgLabels <- argLabelString.split(";").toList.map(
            s => DiscreteLabel.fromRenderedString(s.lowerCase): Either[String, DiscreteLabel]
          ).sequence[Either[String, ?], DiscreteLabel].swap.map(_ + s" - Line: $line").swap
        } yield {
          // TODO reimplement for new discrete labels
          val questions: List[String] = ???
          //DiscreteLabel.getQuestionsForDiscreteLabels(sentenceTokens, verbInflectedForms, discreteArgLabels)
          //.map(_.getOrElse("N/A"))
          if(useSlots) {
            questions.zip(
              SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
                sentenceTokens, verbInflectedForms, questions
              )
            ).map {
              case (q, None) =>
                System.err.println(s"Could not get slots for question: $q")
                "N/A"
              case (_, Some(slots)) =>
                slots.toString
            }
          } else {
            questions
          }
        }
        resEith match {
          case Left(msg) =>
            System.err.println(msg)
            System.out.println // empty output
          case Right(questions) =>
            System.out.println(questions.mkString(";"))
        }
      }
    }
  }
)
