package mts.experiments.expG

import mts.util._
import mts.datasets.conll._

trait PackagePlatformExtensions {
  case class SavedManualQARecord(
    path: CoNLLSentencePath,
    qaGroups: List[List[KeywordedQAPair]]
  ) {
    def sentence: CoNLLSentence = FileManager.getCoNLLSentence(path).get

    def load: ManualQARecord = ManualQARecord(path, sentence, qaGroups)
  }
  object SavedManualQARecord {
    def blank(path: CoNLLSentencePath, sentence: CoNLLSentence) =
      SavedManualQARecord(path, List(List(KeywordedQAPair.blank)))
  }
}
