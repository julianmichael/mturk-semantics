package turksem.qasrl

/** Data structure to keep track of a single worker's stats on the validation task. */
case class QASRLValidationWorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numComparisonInstances: Int,
  numComparisonAgreements: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  numRedundants: Int,
  timeSpent: Long,
  earnings: Double,
  warnedAt: Option[Int],
  blockedAt: Option[Int]) {

  def agreement = numComparisonAgreements.toDouble / numComparisonInstances

  def addAssignment(response: List[QASRLValidationAnswer], timeTaken: Long, totalReward: Double) = this.copy(
    numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
    numAnswerSpans = this.numAnswerSpans + response.filter(_.isAnswer).size,
    numInvalids = this.numInvalids + response.filter(_.isInvalid).size,
    numRedundants = this.numRedundants + response.filter(_.isRedundant).size,
    timeSpent = this.timeSpent + timeTaken,
    earnings = this.earnings + totalReward)

  def addComparison(numTotal: Int, numAgreed: Int) = this.copy(
    numComparisonInstances = this.numComparisonInstances + numTotal,
    numComparisonAgreements = this.numComparisonAgreements + numAgreed
  )

  def warned = this.copy(warnedAt = Some(numAssignmentsCompleted))
  def blocked = this.copy(blockedAt = Some(numAssignmentsCompleted))
}

object QASRLValidationWorkerInfo {
  def empty(workerId: String) = QASRLValidationWorkerInfo(workerId, 0, 0, 0, 0, 0, 0, 0L, 0.0, None, None)
}
