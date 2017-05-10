package turkey

import scala.util.Try

import upickle.default.Writer
import upickle.default.Reader

// NOTE: could parametrize this over a monad in the future, if needed
trait HITDataService {

  def saveHIT[Prompt : Writer](
    hit: HIT[Prompt]
  ): Try[Unit]

  def getHIT[Prompt : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[HIT[Prompt]]

  def saveApprovedAssignment[Response : Writer](
    assignment: Assignment[Response]
  ): Try[Unit]

  def saveRejectedAssignment[Response : Writer](
    assignment: Assignment[Response]
  ): Try[Unit]

  def getHITInfo[Prompt: Reader, Response : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[HITInfo[Prompt, Response]]

  def getAllHITInfo[Prompt: Reader, Response : Reader](
    hitTypeId: String
  ): Try[List[HITInfo[Prompt, Response]]]

  def getAssignmentsForHIT[Response : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[List[Assignment[Response]]]

}
