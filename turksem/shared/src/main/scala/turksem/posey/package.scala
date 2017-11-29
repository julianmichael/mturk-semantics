package turksem

import spacro.tasks.ResponseRW

import turksem.util.ContiguousSpan
import turksem.util._

import cats.data.NonEmptyList
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.PosTags

package object posey {

  val poseyTaskKey = "posey"

  sealed trait PairJudgment
  case object BadPair extends PairJudgment
  case class PairQA(question: String, answer: ContiguousSpan) extends PairJudgment

  case class PoseyPrompt[SID](
    sid: SID,
    pairs: NonEmptyList[(ContiguousSpan, ContiguousSpan)]
  )

  case class PoseyResponse(qas: List[PairJudgment])

  case class PoseyAjaxRequest[SID](id: SID) {
    type Response = PoseyAjaxResponse
  }
  object PoseyAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] =
      new ResponseRW[PoseyAjaxRequest[SID]] {
        override def getReader(request: PoseyAjaxRequest[SID]) = implicitly[Reader[PoseyAjaxResponse]]
        override def getWriter(request: PoseyAjaxRequest[SID]) = implicitly[Writer[PoseyAjaxResponse]]
      }
  }

  case class PoseyAjaxResponse(tokens: Vector[String])
}
