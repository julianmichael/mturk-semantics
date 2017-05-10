package nlpdata.datasets.wiki1k

import cats.Monad
import cats.syntax.all._

trait Wiki1kService[M[_]] {
  implicit def monad: Monad[M]

  def getFile(path: Wiki1kPath): M[Wiki1kFile]

  def getSentence(path: Wiki1kSentencePath): M[Wiki1kSentence] =
    getFile(path.filePath).map(_.paragraphs(path.paragraphNum)(path.sentenceNum))
}
