package nlpdata.datasets.nombank

import cats.Monad
import cats.implicits._

import nlpdata.util._
import nlpdata.structure._
import nlpdata.datasets.ptb._

class NomBankRawService[M[_]](
  val nombank: collection.Map[PTBSentencePath, List[NomBankEntry]],
  ptbService: PTBService[M])(
  implicit override val monad: Monad[M]
) extends NomBankService[M] {

  override def getPredArgStructures(
    path: PTBSentencePath
  ): M[List[PredicateArgumentStructure]] =
    ptbService.getSentence(path).map { sentence =>
      nombank(path).map(getPredicateArgumentStructure(_, sentence.syntaxTree))
    }

  override def getPredArgStructuresReindexed(
    path: PTBSentencePath
  ): M[List[PredicateArgumentStructure]] =
    ptbService.getSentence(path).map { sentence =>
      nombank(path).map(getPredicateArgumentStructureReindexed(_, sentence.syntaxTree))
    }
}
