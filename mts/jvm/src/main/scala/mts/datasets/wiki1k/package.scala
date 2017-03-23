package mts.datasets.wiki1k

import mts.util._

import java.nio.file.{Paths, Path, Files}
import java.io.{BufferedReader, StringReader}

import scala.util.{Try, Success, Failure}

import edu.stanford.nlp.process.DocumentPreprocessor;

trait PackagePlatformExtensions {

  import scala.language.implicitConversions
  implicit def fileManagerToWiki1k(fm: FileManager.type) = Wiki1kFileManager

  object Wiki1kFileManager {
    private[this] val wiki1kDatasetPath = Paths.get("wiki1k")

    import com.softwaremill.macmemo.memoize
    import com.softwaremill.macmemo.MemoCacheBuilder
    implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

    @memoize(maxSize = 900, expiresAfter = 1 hour)
    private[this] def getWiki1kFileUnsafe(path: Wiki1kPath): Wiki1kFile = {
      val fullPath = wiki1kDatasetPath.resolve(path.get)
      val fileResource = for {
        lines <- FileManager.loadResource(fullPath)
        file = Parsing.readFile(path, lines)
      } yield file
      fileResource.tried.get
    }

    def getWiki1kFile(path: Wiki1kPath): Try[Wiki1kFile] =
      Try(getWiki1kFileUnsafe(path))

    def getWiki1kSentence(path: Wiki1kSentencePath): Try[Wiki1kSentence] = for {
      file <- getWiki1kFile(path.filePath)
    } yield file.paragraphs(path.paragraphNum)(path.sentenceNum)

    val txtRegex = """^(.*).txt$""".r

    @memoize(maxSize = 2, expiresAfter = 1 hour)
    def wiki1kPathsForDomain(domain: String): Vector[Wiki1kPath] = {
      val prefix = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
      val domainFile = new java.io.File(prefix.resolve(domain).toString)
      val allPaths = for {
        file <- domainFile.listFiles.iterator
        if !file.isDirectory && txtRegex.pattern.matcher(file.getName).matches
        txtRegex(id) = file.getName
      } yield Wiki1kPath(domain, id)
      allPaths.toVector
    }

    lazy val allWiki1kPaths = wiki1kDomains.flatMap(wiki1kPathsForDomain)

    lazy val allWiki1kSentencePaths = allWiki1kPaths.flatMap { wiki1kPath =>
      getWiki1kFile(wiki1kPath).get.paragraphs.iterator.flatten.map(_.path)
    }

    def wikipediaIds = {
      val pageBeginRegex = """^###doc id="(.*)" url="(.*)" title="(.*)".*$""".r
      val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
      val sourceFilePath = wiki1kFullPath.resolve("Wiki1000/wiki1000.txt")

      import scala.collection.JavaConverters._
      Files.lines(sourceFilePath).iterator.asScala.collect {
        case pageBeginRegex(id, url, title) => id
      }.toList
    }

    def wikinewsIds = {
      val pageBeginRegex = """^###doc id="(.*)" url="(.*)" title="(.*)".*$""".r
      val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
      val sourceFilePath = wiki1kFullPath.resolve("wikinews.txt")

      import scala.collection.JavaConverters._
      Files.lines(sourceFilePath).iterator.asScala.collect {
        case pageBeginRegex(id, url, title) => id
      }.toVector.sortBy(-_.toInt).drop(500).toList
    }

    // domain: "wikipedia" or "wikinews" --- it is used for the API URL too!
    // options: list of page IDs
    def createWikiDataset(
      domain: String,
      pageIds: List[String],
      size: Int
    ) = {
      import argonaut._
      import Argonaut._

      def urlForPageId(pageId: String) = {
        val urlPageId = pageId.replaceAll(" ", "%20")
        s"https://en.$domain.org/w/api.php?action=query&prop=extracts%7Crevisions&format=json&explaintext=true&exsectionformat=plain&pageids=$urlPageId"
      }

      def jsonForPageId(pageId: String) = Parse.parse(
        io.Source.fromURL(urlForPageId(pageId)).mkString
      ).right.get

      // kind of superfluous, oh well
      def idForJson(json: Json) = json
        .fieldOrNull("query").fieldOrNull("pages")
        .objectFields.get.head

      def revIdForJson(json: Json) = json
        .fieldOrNull("query").fieldOrNull("pages")
        .fieldOrNull(idForJson(json))
        .fieldOrNull("revisions")
        .array.get.head
        .fieldOrNull("revid")
      def titleForJson(json: Json) = json
        .fieldOrNull("query").fieldOrNull("pages")
        .fieldOrNull(idForJson(json))
        .fieldOrNull("title")
        .string.get
      def contentForJson(json: Json) = json
        .fieldOrNull("query").fieldOrNull("pages")
        .fieldOrNull(idForJson(json))
        .fieldOrNull("extract")
        .string.get

      def writeFileFromPageId(pageId: String) = {
        val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
        val domainPath = wiki1kFullPath.resolve(domain)
        if(!Files.exists(domainPath)) {
          Files.createDirectories(domainPath)
        }

        val json = jsonForPageId(pageId)
        val id = idForJson(json)
        val revId = revIdForJson(json)
        val title = titleForJson(json)
        val content = contentForJson(json)
        val fullParagraphs = content.split("\\n+")
        import scala.collection.JavaConverters._
        val paragraphs = fullParagraphs.iterator
          .takeWhile(line => !Set("See also", "References", "Sources").contains(line.trim)
        ).map { pLine =>
          new DocumentPreprocessor(
            new BufferedReader(new StringReader(pLine))
          ).iterator.asScala.map { tokenListJava =>
            tokenListJava.iterator.asScala.map(_.word).toVector: Vector[String]
          }.toVector
        }.filter(_.size > 1).toVector
        // ^^ remove paragraphs with 1 or fewer lines. gets rid of titles, lists, etc
        val fullPath = wiki1kFullPath.resolve(Wiki1kPath(domain, id).get)
        val contentString = paragraphs.map(sentences =>
          sentences.map(sentence =>
            sentence.mkString(" ")
          ).mkString("\n")
        ).mkString("\n\n")
        val fileString = s"$id\n$revId\n$title\n$contentString"
        if(Files.exists(fullPath)) {
          System.err.println(s"File already exists: $title ($id)")
        }
        Try(Files.write(fullPath, fileString.getBytes)).toOptionPrinting
      }

      pageIds.iterator.map { pageId =>
        Thread.sleep(100)
        Try(writeFileFromPageId(pageId)) match {
          case Success(_) => true
          case Failure(_) => println(s"Missing $domain page ID: $pageId"); false
        }
      }.filter(identity).take(size).foreach(_ => ())
    }
  }
}
