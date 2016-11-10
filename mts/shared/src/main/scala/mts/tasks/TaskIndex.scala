package mts.tasks

// == THE TASK INDEX ==
// you must place a label for your task here
// in order for it to work.
object TaskIndex {
  sealed trait TaskLabel {
    type Prompt
    type Response
  }
  case object SampleTaskLabel extends TaskLabel {
    type Prompt = mts.experiments.sample.SamplePrompt
    type Response = mts.experiments.sample.SampleResponse
  }
}
