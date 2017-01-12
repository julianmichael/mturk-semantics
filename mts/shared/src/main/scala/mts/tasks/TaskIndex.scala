package mts.tasks

import upickle.default._

// == THE TASK INDEX ==
// you must place a label for your task here
// in order for it to work.
// TODO ok... not yet actually. maybe not ever. we will see
// object TaskIndex {
//   sealed trait TaskKey {
//     type Prompt
//     type Response
//     type ApiRequest
//     type ApiResponse
//     def getSerializationGuru: TaskSerializationGuru[Prompt, Response, ApiRequest, ApiResponse]
//   }
//   case object SampleTaskKey extends TaskKey {
//     import mts.experiments.sample._
//     type Prompt = SamplePrompt
//     type Response = SampleResponse
//     type ApiRequest = ApiRequest
//     type ApiResponse = ApiResponse
//   }

//   case class TaskSerializationGuru[
//     Prompt, Response, ApiRequest, ApiResponse](
//     implicit val requestReader: Reader[ApiRequest],
//     val responseWriter: Writer[ApiResponse])
// }
