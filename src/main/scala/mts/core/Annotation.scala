package mts.core

import mts.analysis.AnnotationStat

case class Annotation(
  assignmentId: String,
  hitType: String,
  hitId: String,
  question: Option[Question],
  workerId: String,
  answer: String,
  // creationTime: Long
  acceptTime: Long,
  submitTime: Long
)

object Annotation {
  // omits question and answer fields since those won't be readily analyzed in R
  def toTSV(as: List[Annotation], stats: List[AnnotationStat]): String = {
    val extraStatHeaders = stats.map(_.label).mkString("\t")
    val header = s"assignmentId\thitType\thitId\tworkerId\tacceptTime\tsubmitTime\t$extraStatHeaders\n"
    val computedStats = stats.map(_.compute(as)).transpose
    def line(a: Annotation, stats: Iterable[String]) = {
      import a._
      val extraStats = stats.mkString("\t")
      s"$assignmentId\t$hitType\t$hitId\t$workerId\t$acceptTime\t$submitTime\t$extraStats\n"
    }
    val sb = new java.lang.StringBuilder
    sb.append(header)
    as.zip(computedStats).foreach { case (annotation, stats) => sb.append(line(annotation, stats)) }
    sb.toString
  }
}
