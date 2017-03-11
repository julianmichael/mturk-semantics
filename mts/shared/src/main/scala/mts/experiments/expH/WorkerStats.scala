package mts.experiments.expH

// for generation task
case class WorkerStats(
  workerId: String,
  numAssignmentsCompleted: Int,
  numQAPairsWritten: Int,
  numQAPairsValid: Int,
  earnings: Double,
  warnedAt: Option[Int],
  blockedAt: Option[Int]) {

  def accuracy = numQAPairsValid.toDouble / numQAPairsWritten

  def addAssignment(
    numWritten: Int,
    numValid: Int,
    totalReward: Double
  ) = WorkerStats(
    workerId,
    numAssignmentsCompleted + 1,
    numQAPairsWritten + numWritten,
    numQAPairsValid + numValid,
    earnings + totalReward,
    warnedAt, blockedAt)

  def warned = this.copy(warnedAt = Some(numAssignmentsCompleted))
  def blocked = this.copy(blockedAt = Some(numAssignmentsCompleted))
}
object WorkerStats {
  def empty(workerId: String) = WorkerStats(workerId, 0, 0, 0, 0.0, None, None)
}
