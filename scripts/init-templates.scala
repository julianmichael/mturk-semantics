import cats._
import cats.data._
import cats.implicits._
import qamr.emnlp2017._
import qamr.emnlp2017.analysis._
val trainTA = new TemplateAnalysis(Datasets.train)
val devTA = new TemplateAnalysis(Datasets.dev)
def loadDevSentence(i: Int) = {
  val id = Datasets.dev.all(i).id.sentenceId
  val alignments = Datasets.dev.all.filter(_.id.sentenceId == id).map(devTA.templatizeQuestionContiguousSlotsWithAlignment)
  (id, alignments)
}

def writeTemplates[Slot : Show](
  filename: String,
  alignmentsBySentence: Map[SentenceId, List[QuestionTemplateAlignment[Slot]]],
  allowedTemplates: Set[QuestionTemplate[Slot]]
): Unit = {
  val templateTSV = makeTemplateTSV(alignmentsBySentence, allowedTemplates)
  qamr.emnlp2017.saveOutputFile(filename, templateTSV)
}

def writeTrainTemplates: Unit = writeTemplates("train-templates.tsv", trainTA.alignmentsBySentence, trainTA.mostCommonTemplates)
def writeDevTemplates: Unit = writeTemplates("dev-templates.tsv", devTA.alignmentsBySentence, trainTA.mostCommonTemplates)

def writeAllTemplates: Unit = {
  val numCataloguedTemplates = trainTA.mostCommonTemplates.size
  val proportionDevTemplatesCovered = {
    val numCoveredTemplates = devTA.alignmentsByTemplate
      .filter(p => trainTA.mostCommonTemplates.contains(p._1))
      .size
    numCoveredTemplates.toDouble / devTA.alignmentsByTemplate.size
  }
  val proportionDevInstancesCovered = {
    val numCoveredTemplateInstances = devTA.alignmentsByTemplate
      .filter(p => trainTA.mostCommonTemplates.contains(p._1))
      .values.toList.map(_.size).sum
    numCoveredTemplateInstances.toDouble / devTA.allTemplateAlignments.size
  }
  val numOverlappingTemplates = (devTA.mostCommonTemplates intersect trainTA.mostCommonTemplates).size
  val numTrainOnlyTemplates = (trainTA.mostCommonTemplates -- devTA.mostCommonTemplates).size
  val numDevOnlyTemplates = (devTA.mostCommonTemplates -- trainTA.mostCommonTemplates).size
  println(s"Number of templates: $numCataloguedTemplates")
  println(f"Percent templates covered in dev: $proportionDevTemplatesCovered%.2f")
  println(f"Percent instances covered in dev: $proportionDevInstancesCovered%.2f")
  println(s"Number of overlapping templates: $numOverlappingTemplates")
  println(s"Number of train only templates: $numTrainOnlyTemplates")
  println(s"Number of dev only templates: $numDevOnlyTemplates")

  writeTrainTemplates
  writeDevTemplates
}
