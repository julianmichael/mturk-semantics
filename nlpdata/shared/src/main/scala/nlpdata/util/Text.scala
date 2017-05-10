package nlpdata.util

import cats._

/** Provides method(s) for rendering text from a list of tokens. */
object Text {
  private val noSpaceBefore = Set(
    ".", ",", "!", "?", ";", ":", "''",
    "n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d",
    // "''", // TODO hmm maybe, maybe not
    "%", "-", "+",
    "-RRB-", "-RCB-", "-RSB-",
    ")", "]", "}",
    "/.", "/?",
    "°"
  )

  private val noSpaceAfter = Set(
    "``",
    "$", "£", "€",
    "#", "-",
    "-LRB-", "-LCB-", "-LSB-",
    "(", "[", "{"
  )

  // val sentenceEndings = Set(
  //   "'", "\"", ".", ",", "!", "?", ";", ":"
  // )

  /** Normalize a Penn Treebank token to its string representations. */
  def normalizeToken(token: String) = token match {
    case "`" => "'"
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

  import cats._
  import cats.data._
  import cats.implicits._
  import scala.language.higherKinds

  /**
    * Returns a best-effort properly spaced representation of a sequence of tokens.
    * (Bear in mind you need to normalize PTB tokens yourself inside the renderWord parameter.)
    * Allows you to specify how to render spaces and words so you can use this to create interactive DOM elements in JS.
    * And it's M O N A D I C C
    * TODO change to foldable LOL
    */
  def renderM[Word, F[_]: Foldable, M[_] : Monad, Result : Monoid](
    words: F[Word],
    getToken: Word => String,
    spaceFromNextWord: Word => M[Result],
    renderWord: Word => M[Result]): M[Result] = {
    val hasSingleQuoteOnly = words.toList.map(getToken).filter(t => t == "'" || t == "`").size == 1
    words.foldM[M, (Result, Boolean, Boolean, Boolean)]((Monoid[Result].empty, true, false, false)) {
      case ((acc, skipSpace, insideSingleQuotes, insideDoubleQuotes), word) =>
        val token = getToken(word)

        val (skipPrevSpace, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes) =
          if(hasSingleQuoteOnly && token == "'") (
            true, false, false, insideDoubleQuotes
          ) else (
            // skip prev space
            skipSpace ||
              (insideSingleQuotes && token.equals("'")) ||
              (insideDoubleQuotes && (token.equals("''") || token.equals("\""))),
            // skip next space
            noSpaceAfter.contains(normalizeToken(token)) ||
              (!insideSingleQuotes && (token.equals("`") || token.equals("'"))) ||
              (!insideDoubleQuotes && (token.equals("``") || token.equals("\""))),
            // now inside single
            (token.equals("'") || token.equals("`")) ^ insideSingleQuotes,
            // now inside double
            (token.equals("''") || token.equals("``") || token.equals("\"")) ^ insideDoubleQuotes
          )

        if(skipPrevSpace || noSpaceBefore.contains(normalizeToken(token))) {
          for {
            w <- renderWord(word)
          } yield {
            (acc |+| w, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes)
          }
        } else {
          for {
            space <- spaceFromNextWord(word)
            w <- renderWord(word)
          } yield {
            (acc |+| space |+| w, skipNextSpace, nowInsideSingleQuotes, nowInsideDoubleQuotes)
          }
        }
    }.map(_._1)
  }

  def render[Word, M](
    words: Seq[Word],
    getToken: Word => String,
    spaceFromNextWord: Word => M,
    renderWord: Word => M)(
    implicit M: Monoid[M]): M = {
    renderM[Word, List, Id, M](words.toList, getToken, spaceFromNextWord, renderWord)
  }

  /** Convenience method for rendering a sequence of PTB tokens directly to a string. */
  def render(words: Seq[String]): String =
    render[String, String](words, identity, _ => " ", normalizeToken)

  def renderSpan(reference: Seq[String], span: Set[Int]) =
    render(reference.zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
}
