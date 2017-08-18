package turksem.scisrl.scisrlexample

import cats._
import cats.implicits._

import turksem._
import turksem.scisrl._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
import nlpdata.structure.Word
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import upickle.default._

class SciSRLAnnotationSetup(implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")

  val Wiktionary = new WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  val allSentences = List(
    "If caution is taken when approaching wild animals, they will not attack.",
    "Plants require water, light, and lots of love to take root in the ground.",
    "Nocturnal animals prefer to hunt at night below the shade of trees."
  )

  implicit object SentenceIdHasTokens extends HasTokens[SentenceId] {
    def getTokens(id: SentenceId): Vector[String] =
      Tokenizer.tokenize(allSentences(id.index))
  }

  lazy val allIds = (0 until allSentences.size).map(SentenceId(_)).toVector

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  lazy val allPrompts: Vector[SciSRLPrompt[SentenceId]] = allIds.map { id =>
    val verbIndices = PosTagger.posTag(id.tokens).collect {
      case Word(index, pos, token) if PosTags.verbPosTags.contains(pos) =>
        inflections.getInflectedForms(token.lowerCase).map(_ => index)
    }.flatten.toList

    SciSRLPrompt(id, verbIndices)
  }

  lazy val experiment = new SciSRLAnnotationPipeline(allPrompts)
}
