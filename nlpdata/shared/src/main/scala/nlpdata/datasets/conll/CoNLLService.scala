package nlpdata.datasets.conll

import cats.Monad
import cats.implicits._

trait CoNLLService[M[_]] {
  implicit def monad: Monad[M]

  def getFile(path: CoNLLPath): M[CoNLLFile]

  def getSentence(path: CoNLLSentencePath): M[CoNLLSentence] =
    getFile(path.filePath).map(_.sentences(path.sentenceNum))
}
