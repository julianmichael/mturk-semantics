package turksem.qasrl

import spacro.HITInfo

class AgreementTester[SID](
  allValInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]])(
  implicit settings: QASRLSettings
) {

  def getResults = {
    var workerInfos = Map.empty[String, QASRLValidationWorkerInfo].withDefault(QASRLValidationWorkerInfo.empty)
    allValInfos.map(_.assignments).foreach { assignments =>
      // add basic assignment info
      assignments.foreach(a =>
        workerInfos = workerInfos.updated(
          a.workerId,
          workerInfos(a.workerId).addAssignment(
            a.response,
            a.submitTime - a.acceptTime,
            settings.validationReward + settings.validationBonus(a.response.size))
          )
      )
      for(a <- assignments) {
        val comparisons = assignments
          .filter(_ != a)
          .flatMap(oa => (a.response, oa.response, List.fill(oa.response.size)(oa.workerId)).zipped
                     .map(QASRLValidationResponseComparison(_, _, _))
                     .toList
        )
        workerInfos = workerInfos.updated(
          a.workerId,
          workerInfos(a.workerId).addComparisons(comparisons)
        )
      }
    }
    workerInfos
  }
}
