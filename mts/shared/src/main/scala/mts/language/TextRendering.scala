package mts.language

/** Provides method(s) for rendering text from a list of tokens.
  */
object TextRendering {
  val noSpaceBefore = Set(
    ".", ",", "!", "?", ";", ":", "''",
    "n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d",
    // "''", // TODO hmm maybe, maybe not
    "%", "-", "+",
    "-RRB-", "-RCB-", "-RSB-",
    ")", "]", "}",
    "/.", "/?"
  )

  val noSpaceAfter = Set(
    "``", "$", "#", "-",
    "-LRB-", "-LCB-", "-LSB-",
    "(", "[", "{"
  )

  // val sentenceEndings = Set(
  //   "'", "\"", ".", ",", "!", "?", ";", ":"
  // )

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
    case "--" => "-"
    case w => w.replaceAll("\\\\/", "/")
  }

  // TODO restrict to just monoid imports
  import scalaz._
  import Scalaz._

  /**
    * Returns a best-effort properly spaced representation of a sequence of tokens. (Bear in mind you need to normalize PTB tokens.)
    * Allows you to specify how to render spaces and words so you can use this to create interactive DOM elements in JS.
    */
  def renderSentence[Word, M](
    words: Seq[Word],
    getToken: Word => String,
    spaceFromNextWord: Word => M,
    renderWord: Word => M)(
    implicit M: Monoid[M]): M = {
    words.foldLeft((M.zero, true)) {
      case ((acc, skipSpace), word) =>
        val skipNextSpace = noSpaceAfter.contains(normalizeToken(getToken(word)))
        if(skipSpace || noSpaceBefore.contains(normalizeToken(getToken(word)))) {
          (acc |+| renderWord(word), skipNextSpace)
        } else {
          (acc |+| spaceFromNextWord(word) |+| renderWord(word), skipNextSpace)
        }
    }._1
  }

  /** Convenience method for rendering a sequence of PTB tokens directly to a string. */
  def renderSentence(words: Seq[String]): String =
    renderSentence[String, String](words, identity, _ => " ", normalizeToken)

  def renderSpan(reference: Seq[String], span: Set[Int]) =
    renderSentence(reference.zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
}
