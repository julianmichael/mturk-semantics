package mts.analysis

import mts.core.Annotation

case class AnnotationStat(
  label: String,
  compute: (List[Annotation] => List[String]) // must produce lists of same length
)

object AnnotationStat {
  def computeWorkerAssignmentNums(annos: List[Annotation]) = {
    val annosByWorker = annos.groupBy(a => (a.workerId, a.hitType))
    val taskNumByAssignmentId = for {
      (_, workerAnnos) <- annosByWorker
      sortedWorkerAnnos = workerAnnos.sortBy(_.acceptTime)
      (assignmentId, assignmentNum) <- sortedWorkerAnnos.map(_.assignmentId).zip(0 until workerAnnos.size)
    } yield (assignmentId, assignmentNum.toString)
    annos.map(a => taskNumByAssignmentId(a.assignmentId))
  }
  val workerAssignmentNum =
    AnnotationStat("workerAssignmentNum", computeWorkerAssignmentNums)
}
