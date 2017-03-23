package mts.datasets

package object wiki1k extends PackagePlatformExtensions {
  val wiki1kDomains = List("wikipedia", "wikinews")

  case class Wiki1kPath(domain: String, suffix: String) {
    def get: String = s"$domain/$suffix.txt"
  }
  case class Wiki1kSentencePath(filePath: Wiki1kPath, paragraphNum: Int, sentenceNum: Int)

  case class Wiki1kFile(
    path: Wiki1kPath,
    id: String,
    revId: String,
    title: String,
    paragraphs: Vector[Vector[Wiki1kSentence]])
  case class Wiki1kSentence(path: Wiki1kSentencePath, tokens: Vector[String])

  import mts.language._
  implicit class Wiki1kTextRendering(val tr: TextRendering.type) extends AnyVal {
    def getTokens(sentence: Wiki1kSentence): Vector[String] = {
      sentence.tokens
    }

    def renderSentence(sentence: Wiki1kSentence) = {
      tr.renderSentence(getTokens(sentence))
    }

    def renderSpan(sentence: Wiki1kSentence, span: Set[Int]) = {
      tr.renderSentence(getTokens(sentence).zipWithIndex.filter(p => span.contains(p._2)).map(_._1))
    }
  }

  object Parsing {
    def readFile(path: Wiki1kPath, lines: Iterator[String]): Wiki1kFile = {
      val id = lines.next
      val revId = lines.next
      val title = lines.next
      def makeParagraph(paragraphNum: Int, lines: List[String]) = lines.reverse.zipWithIndex.map {
        case (line, index) => Wiki1kSentence(Wiki1kSentencePath(path, paragraphNum, index), line.split(" ").toVector)
      }.toVector
      val paragraphs = {
        val (mostPs, lastParagraphNum, extraLines) = lines.foldLeft((List.empty[Vector[Wiki1kSentence]], 0, List.empty[String])) {
          case ((curParagraphs, curParagraphNum, curLines), nextLine) => if(nextLine.isEmpty) {
            val curSentences = makeParagraph(curParagraphNum, curLines)
            (curSentences :: curParagraphs, curParagraphNum + 1, Nil)
          } else {
            (curParagraphs, curParagraphNum, nextLine :: curLines)
          }
        }
        if(extraLines.isEmpty) mostPs.reverse.toVector else {
          val newP = makeParagraph(lastParagraphNum, extraLines)
          (newP :: mostPs).reverse.toVector
        }
      }
      Wiki1kFile(path, id, revId, title, paragraphs)
    }
  }
}
