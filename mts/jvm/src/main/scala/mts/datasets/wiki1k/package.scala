package mts.datasets.wiki1k

import mts.util._

import java.nio.file.{Paths, Path, Files}
import java.io.{BufferedReader, StringReader}

import scala.util.Try

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

    def wikipediaTitles = {
      val pageBeginRegex = """^###doc id="(.*)" url="(.*)" title="(.*)".*$""".r
      val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
      val sourceFilePath = wiki1kFullPath.resolve("Wiki1000/wiki1000.txt")

      import scala.collection.JavaConverters._
      Files.lines(sourceFilePath).iterator.asScala.collect {
        case pageBeginRegex(id, url, title) => title
      }.toList
    }

    def createWikipediaDataset(titles: List[String]) = {
      import argonaut._
      import Argonaut._

      def urlForTitle(searchTitle: String) = {
        val urlTitle = searchTitle.replaceAll(" ", "%20")
        s"https://en.wikipedia.org/w/api.php?action=query&prop=extracts&format=json&explaintext=true&exsectionformat=plain&titles=$urlTitle"
      }

      def jsonForTitle(title: String) = Parse.parse(io.Source.fromURL(urlForTitle(title)).mkString).right.get

      def idForJson(json: Json) = json
        .fieldOrNull("query").fieldOrNull("pages")
        .objectFields.get.head
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

      def writeFileFromTitle(searchTitle: String) = {
        val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
        val wikipediaDomainPath = wiki1kFullPath.resolve("wikipedia")
        if(!Files.exists(wikipediaDomainPath)) {
          Files.createDirectories(wikipediaDomainPath)
        }

        val json = jsonForTitle(searchTitle)
        val id = idForJson(json)
        val title = titleForJson(json)
        val content = contentForJson(json)
        val fullParagraphs = content.split("\\n+")
        import scala.collection.JavaConverters._
        val paragraphs = fullParagraphs.iterator
          .takeWhile(line =>
          !(line.trim.equals("See also") || line.trim.equals("References"))
        ).map { pLine =>
          new DocumentPreprocessor(
            new BufferedReader(new StringReader(pLine))
          ).iterator.asScala.map { tokenListJava =>
            tokenListJava.iterator.asScala.map(_.word).toVector: Vector[String]
          }.toVector
        }.filter(_.size > 1).toVector
        // ^^ remove paragraphs with 1 or fewer lines. gets rid of titles, lists, etc
        val fullPath = wiki1kFullPath.resolve(Wiki1kPath("wikipedia", id).get)
        val contentString = paragraphs.map(sentences =>
          sentences.map(sentence =>
            sentence.mkString(" ")
          ).mkString("\n")
        ).mkString("\n\n")
        val fileString = s"$id\n$title\n$contentString"
        if(Files.exists(fullPath)) {
          System.err.println(s"File already exists: $title ($id)")
        }
        Try(Files.write(fullPath, fileString.getBytes)).toOptionPrinting
      }

      titles.foreach { t => Thread.sleep(500); writeFileFromTitle(t) }
    }

    // slightly weird code because it was originally domain-agnostic before we did mediawiki api queries
    // for wikipedia instead.
    def createWikinewsDataset = {
      val domain = "wikinews"
      val sourceFilePath = "wikinews.txt"
      // make wikipedia directory
      val wiki1kFullPath = FileManager.getResourcePath.resolve(wiki1kDatasetPath)
      val wikipediaDomainPath = wiki1kFullPath.resolve(domain)
      if(!Files.exists(wikipediaDomainPath)) {
        Files.createDirectories(wikipediaDomainPath)
      }
      val filePath = wiki1kFullPath.resolve(sourceFilePath)
      case class FileProps(id: String, title: String)
      val pageBeginRegex = """^###doc id="(.*)" url="(.*)" title="(.*)".*$""".r
      var curProps: FileProps = null
      var curLines: List[String] = Nil
      import scala.collection.JavaConverters._

      // For NEW below. XXX probably not using the NEW style. But it's useful for finding weird things in the data.
      // import edu.stanford.nlp.ling.CoreLabel
      // import edu.stanford.nlp.process.PTBTokenizer
      // import edu.stanford.nlp.process.WordToSentenceProcessor
      // val tokenizerFactory = PTBTokenizer.factory(false, true)
      // val sentenceSplitter = new WordToSentenceProcessor[CoreLabel]

      def tryWriteFile = {
        if(!curLines.isEmpty) {
          val FileProps(id, title) = curProps
          val paragraphs = curLines.iterator.map { pLine =>
            // NEW: sentences printed as non-tokenized strings. need to change reader code to re-tokenize then
            // val allTokenListJ = tokenizerFactory.getTokenizer(new StringReader(pLine), "untokenizable=firstKeep").tokenize
            // val sentences = sentenceSplitter.wordsToSentences(allTokenListJ).iterator.asScala
            //   .map(_.iterator.asScala.toVector)
            //   .toVector
            // val rendered = sentences.map(sentence =>
            //   sentence.map { w =>
            //     val bf = if(w.originalText.trim.equals(",") || w.originalText.trim.equals(".")) ""
            //              else w.before
            //     bf + w.originalText
            //   }.mkString.trim
            // ).mkString(" ")
            // if(!pLine.equals(rendered)) {
            //   println("Non-equal:\noriginal:\n" + pLine)
            //   println("New:\n" + rendered)
            // }
            // sentences
            // OLD: tokens split by spaces
            new DocumentPreprocessor(
              new BufferedReader(new StringReader(pLine))
            ).iterator.asScala.map { tokenListJava =>
              tokenListJava.iterator.asScala.map(_.word).toVector: Vector[String]
            }.toVector
          }.toVector.reverse
          val fullPath = wiki1kFullPath.resolve(Wiki1kPath(domain, id).get)
          val contentString = paragraphs.map(sentences =>
            sentences.map(sentence =>
              sentence.mkString(" ")
            ).mkString("\n")
          ).mkString("\n\n")
          val fileString = s"$id\n$title\n" + contentString
          if(Files.exists(fullPath)) {
            System.err.println(s"File already exists: $curProps")
          }
          Try(Files.write(fullPath, fileString.getBytes)).toOptionPrinting
          curLines = Nil
        }
      }
      Files.lines(filePath).iterator.asScala.foreach { line =>
        line.trim match {
          case "" => tryWriteFile
          case pageBeginRegex(id, url, title) =>
            tryWriteFile
            curProps = FileProps(id, title)
          case paragraph =>
            curLines = line :: curLines
        }
      }
    }
  }
}
