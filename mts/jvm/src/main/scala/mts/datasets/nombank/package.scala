package mts.datasets
package nombank

import mts.util.FileManager
import ptb._

import scala.util.Try

import java.nio.file.Paths

trait PackagePlatformExtensions {

  import scala.language.implicitConversions
  implicit def fileManagerToNomBank(fm: FileManager.type): NomBankFileManager.type = NomBankFileManager

  object NomBankFileManager {
    private[this] val nomBankAnnotationPath = Paths.get("nombank.1.0/nombank.1.0")

    import com.softwaremill.macmemo.memoize
    import com.softwaremill.macmemo.MemoCacheBuilder
    implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

    // this is stupid and due to the lack of a proper builder for immutable.Map
    // @memoize(maxSize = 1, expiresAfter = 1 hour)
    private[this] def getNomBankUnsafe: collection.Map[PTBSentencePath, List[NomBankEntry]] = {
      val fileResource = FileManager.loadResource(nomBankAnnotationPath).map { lines =>
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

    def getPredArgStructures(path: PTBSentencePath): Try[List[PredicateArgumentStructure]] = for {
      sentence <- FileManager.getPTBSentence(path)
      nombank <- getNomBank
    } yield nombank(path).map(getPredicateArgumentStructure(_, sentence.syntaxTree))
  }
}
