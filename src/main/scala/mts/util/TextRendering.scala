package mts.util

/** Provides method(s) for rendering text from a list of tokens. */
object TextRendering {
  val noSpaceBefore = Set(
    ".", ",", "!", "?", ";", ":", "''",
    "n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d",
    "%", "-", "+",
    "-RRB-", "-RCB-", "-RSB-",
    ")", "]", "}",
    "/.", "/?"
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
    case "/." => "."
    case "/?" => "?"
    case w => w.replaceAll("\\/", "/")
  }

  /** Returns a best-effort string representation of a sequence of PTB-style tokens. */
  def renderSentence(sentence: Seq[String]): String = {
    val sentenceBits = sentence.foldLeft((List.empty[String], true)) {
      case ((strings, skipSpace), word) =>
        val skipNextSpace = noSpaceAfter.contains(word)
        if(skipSpace || noSpaceBefore.contains(word)) {
          (normalizeToken(word) :: strings, skipNextSpace)
        } else {
          (normalizeToken(word) :: space :: strings, skipNextSpace)
        }
    }._1.reverse
    sentenceBits.mkString("")
  }
}
