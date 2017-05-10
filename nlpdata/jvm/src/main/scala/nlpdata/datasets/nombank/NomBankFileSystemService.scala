package nlpdata.datasets.nombank

import nlpdata.util._
import nlpdata.structure._
import nlpdata.datasets.ptb._

import scala.util.Try

import cats._
import cats.implicits._

import java.nio.file.{Paths, Path, Files}

class NomBankFileSystemService(
  location: Path,
  ptbService: PTBService[Try]
) extends NomBankService[Try] {

  implicit override val monad: Monad[Try] = implicitly[Monad[Try]]

  private[this] val nomBankAnnotationPath = location.resolve("nombank.1.0")

  // this is stupid and due to the lack of a proper builder for immutable.Map
  lazy val getNomBankUnsafe: collection.Map[PTBSentencePath, List[NomBankEntry]] = {
    val fileResource = loadFile(nomBankAnnotationPath).map { lines =>
      val map = collection.mutable.Map.empty[PTBSentencePath, List[NomBankEntry]]
      lines.foreach { line =>
        val entry = Parsing.readEntry(line)
        val curEntries = map.get(entry.ptbSentencePath).getOrElse(Nil)
        map.put(entry.ptbSentencePath, entry :: curEntries)
      }
      map.toMap.withDefaultValue(Nil)
    }
    fileResource.tried.get
  }

  def getNomBank: Try[collection.Map[PTBSentencePath, List[NomBankEntry]]] =
    Try(getNomBankUnsafe)

  def getRawService: Try[NomBankRawService[Try]] = getNomBank.map(new NomBankRawService(_, ptbService))

  override def getPredArgStructures(
    path: PTBSentencePath
  ): Try[List[PredicateArgumentStructure]] =
    getRawService >>= (_.getPredArgStructures(path))

  override def getPredArgStructuresReindexed(
    path: PTBSentencePath
  ): Try[List[PredicateArgumentStructure]] =
    getRawService >>= (_.getPredArgStructuresReindexed(path))
}
