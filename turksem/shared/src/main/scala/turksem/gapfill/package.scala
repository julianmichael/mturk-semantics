package turksem

import spacro.tasks.ResponseRW

import turksem.util._

import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags

package object gapfill {

  object LowerCaseStringSerialization {
    import nlpdata.util.LowerCaseStrings._

    implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
      case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
    }
    implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
      case s => upickle.Js.Str(s.toString)
    }
  }

  import LowerCaseStringSerialization._

  val gapfillTaskKey = "gapfill"

  case class GapfillPrompt[SID](id: SID)

  case class GapfillResponse(qas: List[(InflectionalWord, JudgedQuestion)])

  case class GapfillAjaxRequest[SID, ClientGuesser](id: SID) {
    type Response = GapfillAjaxResponse[ClientGuesser]
  }
  object GapfillAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID, ClientGuesser : Reader : Writer] =
      new ResponseRW[GapfillAjaxRequest[SID, ClientGuesser]] {
        val rw = macroRW[GapfillAjaxResponse[ClientGuesser]]
        override def getReader(request: GapfillAjaxRequest[SID, ClientGuesser]) = rw
        override def getWriter(request: GapfillAjaxRequest[SID, ClientGuesser]) = rw
      }
  }

  case class GapfillAjaxResponse[ClientGuesser](
    inflectedTokens: Vector[InflectionalWord],
    questionGuesser: ClientGuesser)
}
