package example.multitask

import nlpdata.datasets.ptb3.PTB3SentencePath
import nlpdata.datasets.ptb3.PTB3Path
import nlpdata.datasets.ptb3.WSJPath
import nlpdata.datasets.ptb3.BrownPath
import nlpdata.datasets.conll.CoNLLPath
import nlpdata.datasets.conll.CoNLLSentencePath

sealed trait SentenceId {
  def getPTB = Some(this) collect { case id @ PTBSentenceId(_) => id }
  def getCoNLL = Some(this) collect { case id @ CoNLLSentenceId(_) => id }

  def isPTB = getPTB.nonEmpty
  def isCoNLL = getCoNLL.nonEmpty
}
case class PTBSentenceId(path: PTB3SentencePath) extends SentenceId
case class CoNLLSentenceId(path: CoNLLSentencePath) extends SentenceId

object SentenceId {

  def isBrownTest(id: PTBSentenceId) = id.path.filepath match {
    case BrownPath("CK", i) if i <= 3 => true
    case _ => false
  }

  def isBrownDev(id: PTBSentenceId) = id.path.filepath match {
    case BrownPath("CK", i) if i > 3 && i <= 6 => true
    case _ => false
  }

  def isBrownTrain(id: PTBSentenceId) = id.path.filepath match {
    case BrownPath("CK", i) if i > 6 => true
    case _ => false
  }

  // not necessarily used for serialization over the wire, but
  // used for storing to / reading from  the dataset file.
  def toString(sid: SentenceId): String = sid match {
    case PTBSentenceId(PTB3SentencePath(WSJPath(section, number), sentenceNum)) =>
      f"WSJ:$section%02d$number%02d:$sentenceNum%d"
    case PTBSentenceId(PTB3SentencePath(BrownPath(domain, number), sentenceNum)) =>
      f"Brown:$domain%s$number%02d:$sentenceNum%d"
    case CoNLLSentenceId(path) => s"CoNLL:${path.filePath.suffix}:${path.sentenceNum}"
  }

  private[this] val WSJMatch = "WSJ:([0-9]{2})([0-9]{2}):([0-9]+)".r
  private[this] val BrownMatch = "Brown:([^:]{2})([0-9]{2}):([0-9]+)".r
  private[this] val CoNLLMatch = "CoNLL:([^:]+):([0-9]+)".r
  private[this] object int {
    def unapply(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
  }

  def fromString(s: String): SentenceId = s match {
    case WSJMatch(int(section), int(number), int(sentenceNum)) =>
      PTBSentenceId(PTB3SentencePath(WSJPath(section, number), sentenceNum))
    case BrownMatch(domain, int(number), int(sentenceNum)) =>
      PTBSentenceId(PTB3SentencePath(BrownPath(domain, number), sentenceNum))
    case CoNLLMatch(suffix, int(sentenceNum)) =>
      CoNLLSentenceId(CoNLLSentencePath(CoNLLPath.fromPathSuffix(suffix).get, sentenceNum))
  }
}
