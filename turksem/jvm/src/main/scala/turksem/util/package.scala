package turksem.util

import cats.implicits._

import java.io.StringWriter
import java.io.PrintWriter

import scala.util.{Try, Success, Failure}

import com.typesafe.scalalogging.Logger

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

trait PackagePlatformExtensions {
  def sendToClipboard(s: String): Unit = {
    import java.awt._;
    import java.awt.datatransfer._;
    import java.io._;
    val selection = new StringSelection(s)
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, selection)
  }

  implicit class RichTry[A](val t: Try[A]) {
    def toOptionLogging(logger: Logger): Option[A] = t match {
      case Success(a) =>
        Some(a)
      case Failure(e) =>
        val sw = new StringWriter()
        val pw = new PrintWriter(sw, true)
        e.printStackTrace(pw)
        logger.error(e.getLocalizedMessage + "\n" + sw.getBuffer.toString)
        None
    }
  }

  def getInflectionalSentence[SID : HasTokens](sid: SID)(
    implicit inflections: Inflections
  ) = {
    PosTagger.posTag(sid.tokens).map { w =>
      InflectionalWord(
        token = w.token,
        pos = w.pos,
        index = w.index,
        inflectedFormsOpt = inflections.getInflectedForms(w.token.lowerCase)
      )
    }.toVector
  }

}
