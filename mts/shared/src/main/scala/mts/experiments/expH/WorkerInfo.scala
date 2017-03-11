package mts.experiments.expH

// keep track of worker agreement in validation
case class WorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numComparisonInstances: Int,
  numComparisonAgreements: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  numRedundants: Int,
  earnings: Double,
  warnedAt: Option[Int],
  blockedAt: Option[Int]) {

  def agreement = numComparisonAgreements.toDouble / numComparisonInstances

  def addAssignment(response: List[ValidationAnswer], totalReward: Double) = this.copy(
    numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
    numAnswerSpans = this.numAnswerSpans + response.filter(_.isAnswer).size,
    numInvalids = this.numInvalids + response.filter(_.isInvalid).size,
    numRedundants = this.numRedundants + response.filter(_.isRedundant).size,
    earnings = this.earnings + totalReward)

  def addComparison(numTotal: Int, numAgreed: Int) = this.copy(
    numComparisonInstances = this.numComparisonInstances + numTotal,
    numComparisonAgreements = this.numComparisonAgreements + numAgreed
  )

  def warned = this.copy(warnedAt = Some(numAssignmentsCompleted))
  def blocked = this.copy(blockedAt = Some(numAssignmentsCompleted))
}

object WorkerInfo {
  def empty(workerId: String) = WorkerInfo(workerId, 0, 0, 0, 0, 0, 0, 0.0, None, None)
}
