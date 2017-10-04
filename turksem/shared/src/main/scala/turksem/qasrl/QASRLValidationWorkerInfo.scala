package turksem.qasrl

case class QASRLValidationResponseComparison(
  thisResponse: QASRLValidationAnswer,
  thatResponse: QASRLValidationAnswer) {
  def swap = QASRLValidationResponseComparison(thatResponse, thisResponse)
  def isAgreement = (thisResponse, thatResponse) match {
    case (InvalidQuestion, InvalidQuestion) => true
    case (Answer(spans1), Answer(spans2)) =>
      spans1.exists(span1 =>
        spans2.exists(span2 =>
          span1.indices.intersect(span2.indices).nonEmpty
        )
      )
    case _ => false
  }
}

/** Data structure to keep track of a single worker's stats on the validation task. */
case class QASRLValidationWorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  comparisons: List[QASRLValidationResponseComparison],
  numBonusAgreements: Int,
  timeSpent: Long,
  earnings: Double) {

  def numComparisonInstances = comparisons.size

  def rawAccuracyOnInvalids = {
    val compsAgainstInvalid = comparisons.filter(_.thatResponse.isInvalid)
    val numInvalidGuesses = compsAgainstInvalid.filter(_.isAgreement).size
    numInvalidGuesses.toDouble / compsAgainstInvalid.size
  }

  def rawAccuracyOnValids = {
    val compsAgainstValid = comparisons.filter(_.thatResponse.isAnswer)
    val numValidGuesses = compsAgainstValid.filter(_.isAgreement).size
    numValidGuesses.toDouble / compsAgainstValid.size
  }

  def rawAgreement = (rawAccuracyOnInvalids + rawAccuracyOnValids) / 2.0

  def accuracyOnInvalids = {
    val compsAgainstInvalid = comparisons.filter(_.thatResponse.isInvalid)
    val numInvalidGuesses = compsAgainstInvalid.filter(_.isAgreement).size
    (numInvalidGuesses.toDouble + numBonusAgreements) / (compsAgainstInvalid.size + numBonusAgreements)
  }

  def accuracyOnValids = {
    val compsAgainstValid = comparisons.filter(_.thatResponse.isAnswer)
    val numValidGuesses = compsAgainstValid.filter(_.isAgreement).size
    (numValidGuesses.toDouble + numBonusAgreements) / (compsAgainstValid.size + numBonusAgreements)
  }

  def agreement = (accuracyOnInvalids + accuracyOnValids) / 2.0

  def proportionInvalid = numInvalids.toDouble / (numAnswerSpans + numInvalids)

  def addBonusAgreements(n: Int) = this.copy(
    numBonusAgreements = this.numBonusAgreements + n
  )

  def addAssignment(response: List[QASRLValidationAnswer], timeTaken: Long, totalReward: Double) = this.copy(
    numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
    numAnswerSpans = this.numAnswerSpans + response.filter(_.isAnswer).size,
    numInvalids = this.numInvalids + response.filter(_.isInvalid).size,
    timeSpent = this.timeSpent + timeTaken,
    earnings = this.earnings + totalReward)

  def addComparisons(newComparisons: List[QASRLValidationResponseComparison]) = this.copy(
    comparisons = newComparisons ++ this.comparisons
  )
}

object QASRLValidationWorkerInfo {
  def empty(workerId: String) = QASRLValidationWorkerInfo(workerId, 0, 0, 0, Nil, 0, 0L, 0.0)
}
