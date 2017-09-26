package turksem.qasrl

case class QASRLGenerationWorkerStats(
  workerId: String,
  numAssignmentsCompleted: Int,
  numQAPairsWritten: Int,
  numQAPairsValid: Int,
  numBonusValids: Int,
  timeSpent: Long,
  earnings: Double) {

  def accuracy = (numQAPairsValid.toDouble + numBonusValids) / (numQAPairsWritten + numBonusValids)

  def addBonusValids(n: Int) = this.copy(
    numBonusValids = this.numBonusValids + n
  )

  def addAssignment(
    numWritten: Int,
    numValid: Int,
    timeTaken: Long,
    totalReward: Double
  ) = QASRLGenerationWorkerStats(
    workerId,
    numAssignmentsCompleted + 1,
    numQAPairsWritten + numWritten,
    numQAPairsValid + numValid,
    numBonusValids,
    timeSpent + timeTaken,
    earnings + totalReward)

}
object QASRLGenerationWorkerStats {
  def empty(workerId: String) = QASRLGenerationWorkerStats(workerId, 0, 0, 0, 0, 0L, 0.0)
}
