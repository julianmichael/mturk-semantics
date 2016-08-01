import scala.util.{Try, Success, Failure}

package object mts {
  val annotationFilepaths = List(
    "bn/abc/00/abc_0010.v4_gold_conll",
    "mz/sinorama/10/ectb_1010.v4_gold_conll",
    "bc/msnbc/00/msnbc_0000.v4_gold_conll",
    "nw/wsj/24/wsj_2400.v4_gold_conll",
    "nw/xinhua/00/chtb_0010.v4_gold_conll",
    "pt/nt/40/nt_4010.v4_gold_conll",
    // "tc/ch/00/ch_0010.v4_gold_conll",
    "wb/eng/00/eng_0000.v4_gold_conll"
  ).map(CoNLLPath.apply)

  implicit class EnrichedTry[A](t: Try[A]) {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) => Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        None
    }
  }
}
