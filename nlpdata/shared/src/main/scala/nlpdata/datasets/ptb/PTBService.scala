package nlpdata.datasets.ptb

import cats.Monad
import cats.implicits._

trait PTBService[M[_]] {
  implicit def monad: Monad[M]

  def getFile(path: PTBPath): M[PTBFile]

  def getSentence(path: PTBSentencePath): M[PTBSentence] =
    getFile(path.filePath).map(_.sentences(path.sentenceNum))

  def allPTBPaths: M[List[PTBPath]]

  def allPTBSentencePaths: M[List[PTBSentencePath]] = allPTBPaths >>= { ptbPaths =>
    ptbPaths.map { ptbPath =>
      getFile(ptbPath).map { ptbFile =>
        ptbFile.sentences.indices.iterator.map(i => PTBSentencePath(ptbPath, i))
      }
    }.sequence.map(_.flatten)
  }
}
