import mts.tasks._
implicit val config = SandboxTaskConfig("localhost")
import mts.experiments.expE._
val exp = new QuestionWordExperiment
exp.server
