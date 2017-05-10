package turkey.experiments

package object sample {
  case class SamplePrompt(id: Int)
  case class SampleResponse(isGood: Boolean)

  sealed trait ApiRequest
  case class SentenceRequest(id: Int) extends ApiRequest

  sealed trait ApiResponse
  case class SentenceResponse(id: Int, sentence: String) extends ApiResponse

  val sampleTaskKey = "sample"
}
