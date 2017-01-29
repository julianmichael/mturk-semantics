import mts.tasks._
implicit val config = SandboxTaskConfig("localhost")
import mts.experiments.sample._
val exp = new SampleExperiment
val s1Prompt = SamplePrompt(mts.experiments.sentences.head._1)