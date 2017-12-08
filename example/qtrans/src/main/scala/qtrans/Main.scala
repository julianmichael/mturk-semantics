package qtrans

// TODO port code to JS...or something...so can run standalone.
// would require porting the WiktionaryFileSystemService to JS too..

import turksem.qasrl.QALabelMapper

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
        val resEith = for {
          sentenceTokens <- lineArr.lift(0).toRight(
            s"Input line must begin with the sentence tokens followed by ###. Found: $line"
          ).map(_.split(" ").toVector)
          verbStr <- lineArr.lift(1).toRight(
            s"Second entry of input line after ### must be the verb. Found: $line"
          )
          verbInflectedForms <- inflections.getInflectedForms(verbStr.lowerCase).toRight(
            s"Could not get inflections for verb $verbStr."
          )
        } yield {
          val collapsedArgLabels = lineArr.lift(2).getOrElse("").split(";").toList.map(_.lowerCase)
          val questions = QALabelMapper.getQuestionsForCollapsedLabels(verbInflectedForms, collapsedArgLabels)
          if(useSlots) {
            questions.zip(
              QALabelMapper.getVerbAbstractedSlotLabel(
                sentenceTokens, verbInflectedForms, questions.map(_.toString)
              )
            ).map {
              case (q, None) =>
                System.err.println(s"Could not get slots for question: $q")
                "N/A"
              case (_, Some(slots)) =>
                slots.toString
            }
          } else {
            questions.map(_.toString)
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
