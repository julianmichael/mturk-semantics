package nlpdata.datasets.nombank

import cats.Monad
import cats.syntax.all._

import nlpdata.datasets.ptb._
import nlpdata.structure._

trait NomBankService[M[_]] {
  implicit def monad: Monad[M]

  def getPredArgStructures(
    path: PTBSentencePath
  ): M[List[PredicateArgumentStructure]]

  def getPredArgStructuresReindexed(
    path: PTBSentencePath
  ): M[List[PredicateArgumentStructure]]
}
