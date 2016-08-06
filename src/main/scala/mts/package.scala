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

  def conllSentences(): Iterator[(CoNLLSentencePath, String)] = for {
    path <- annotationFilepaths.iterator
    file <- FileManager.getCoNLLFile(path).toOptionPrinting.iterator
    sentence <- file.sentences
    sentenceString = TextRendering.renderSentence(sentence)
  } yield (CoNLLSentencePath(path, sentence.sentenceNum), sentenceString)

  implicit class EnrichedTry[A](t: Try[A]) {
    def toOptionPrinting: Option[A] = t match {
      case Success(a) => Some(a)
      case Failure(e) =>
        System.err.println(e.getLocalizedMessage)
        None
    }
  }

  implicit class EnrichedIterator[A](t: Iterator[A]) {
    def nextOption: Option[A] = if(t.hasNext) Some(t.next) else None
  }

  object TextRendering {

    val noSpaceBefore = Set(
      ".", ",", "!", "?", ";", ":", "''",
      "n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d",
      "%", "-", "+",
      "-RRB-", "-RCB-", "-RSB-",
      ")", "]", "}"
    )

    val noSpaceAfter = Set(
      "``", "$", "#",
      "-LRB-", "-LCB-", "-LSB-",
      "(", "[", "{"
    )

    // val sentenceEndings = Set(
    //   "'", "\"", ".", ",", "!", "?", ";", ":"
    // )

    val space = " "

    def normalizeToken(token: String) = token match {
      case "``" => "\""
      case "''" => "\""
      case "-LRB-" => "("
      case "-RRB-" => ")"
      case "-LCB-" => "{"
      case "-RCB-" => "}"
      case "-LSB-" => "["
      case "-RSB-" => "]"
      case w => w.replaceAll("\\/", "/")
    }

    def renderSentence(sentence: CoNLLSentence): String = {
      def wordSpan(word: CoNLLWord) = {
        normalizeToken(word.token)
      }
      val sentenceBits = sentence.words.foldLeft((List.empty[String], true)) {
        case ((strings, skipSpace), word) =>
          val skipNextSpace = noSpaceAfter.contains(word.token)
          if(skipSpace || noSpaceBefore.contains(word.token)) {
            (wordSpan(word) :: strings, skipNextSpace)
          } else {
            (wordSpan(word) :: space :: strings, skipNextSpace)
          }
      }._1.reverse
      sentenceBits.mkString("")
    }
  }
}
