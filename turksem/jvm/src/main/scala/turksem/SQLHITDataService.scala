package turksem

import turkey.HIT
import turkey.Assignment
import turkey.HITInfo
import turkey.HITDataService

import scala.util.Try

import upickle.default._

// import slick.dbio.Effect.Write
import slick.jdbc.{GetResult, JdbcBackend, JdbcProfile, PostgresProfile}
// import slick.jdbc.JdbcBackend._
// import slick.sql.SqlAction

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

// import slick.jdbc.H2Profile.api._
// import scala.concurrent.ExecutionContext

object SQLHITDataService {
  // TODO provide factory method that constructs database, tables, and other stuff if they are not present
}

class SQLHITDataService(
  val jdbcProfile: JdbcProfile
) extends HITDataService {

  import jdbcProfile.api._

  // TODO possibly move this outside to factory method
  lazy val db: Database = ???

  // data table definitions
  // (could go directly to correct prompt and response IF this were specified further to a particular task)

  class HITs(tag: Tag) extends Table[HIT[String]](tag, "HIT") {
    def hitTypeId = column[String]("hitTypeId") // keys primary key etc
    def hitId = column[String]("hitId") // keys primary key etc
    def prompt = column[String]("prompt")
    def creationTime = column[Long]("creationTime")
    def * = (hitTypeId, hitId, prompt, creationTime) <> ((HIT.apply[String] _).tupled, HIT.unapply[String])
  }
  lazy val hits = TableQuery[HITs]

  class Assignments(tag: Tag) extends Table[Assignment[String]](tag, "Assignment") {
    def hitTypeId = column[String]("hitTypeId") // keys primary key etc
    def hitId = column[String]("hitId") // keys primary key etc
    def assignmentId = column[String]("assignmentId") // keys primary key etc
    def workerId = column[String]("workerId")
    def acceptTime = column[Long]("acceptTime")
    def submitTime = column[Long]("submitTime")
    def response = column[String]("response")
    def feedback = column[String]("feedback")
    def * = (
      hitTypeId, hitId, assignmentId, workerId,
      acceptTime, submitTime, response, feedback) <> ((Assignment.apply[String] _).tupled, Assignment.unapply[String])
  }
  lazy val assignments = TableQuery[Assignments]

  def writeHIT[Prompt: Writer](hit: HIT[Prompt]): HIT[String] =
    hit.copy(prompt = write(hit.prompt))
  def readHIT[Prompt: Reader](hit: HIT[String]): HIT[Prompt] =
    hit.copy(prompt = read[Prompt](hit.prompt))
  def writeAssignment[Response: Writer](assignment: Assignment[Response]): Assignment[String] =
    assignment.copy(response = write(assignment.response))
  def readAssignment[Response: Reader](assignment: Assignment[String]): Assignment[Response] =
    assignment.copy(response = read[Response](assignment.response))

  override def saveHIT[Prompt : Writer](
    hit: HIT[Prompt]
  ): Try[Unit] = {
    val fut = db.run(hits += writeHIT(hit))
    Try(Await.result(fut, 2.seconds))
  }


  import com.softwaremill.macmemo.memoize
  import com.softwaremill.macmemo.MemoCacheBuilder
  private[this] implicit val cacheProvider = MemoCacheBuilder.guavaMemoCacheBuilder

  @memoize(maxSize = 1000, expiresAfter = 1.hour)
  private[this] def getHITUnsafe[Prompt : Reader](
    hitTypeId: String,
    hitId: String
  ): HIT[Prompt] = {
    val fut = db.run(
      hits.filter(hit => hit.hitTypeId === hitTypeId && hit.hitId === hitId)
        .take(1)
        .result)
    readHIT[Prompt](Await.result(fut, 2.seconds).head)
  }

  override def getHIT[Prompt : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[HIT[Prompt]] = Try(getHITUnsafe(hitTypeId, hitId))

  override def saveApprovedAssignment[Response : Writer](
    assignment: Assignment[Response]
  ): Try[Unit] = {
    val fut = db.run(assignments += writeAssignment(assignment))
    Try(Await.result(fut, 2.seconds))
  }

  override def saveRejectedAssignment[Response : Writer](
    assignment: Assignment[Response]
  ): Try[Unit] = {
    // TODO actually do it ... right now I don't care because I never reject.
    // to do this properly we need to use EvaluatedAssignment or something.
    // furthermore we should in that case refactor the api to saveEvaluatedAssignment.
    Try(())
  }

  /** Get a saved HIT and all data relevant to that HIT. */
  def getHITInfo[Prompt: Reader, Response : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[HITInfo[Prompt, Response]] = {
    val query =
      (hits join assignments on (_.hitId === _.hitId))
        .filter(_._1.hitTypeId === hitTypeId)
        .filter(_._1.hitId === hitId)
    val fut = db.run(query.result)
    Try {
      val result = Await.result(fut, 2.seconds)
      val assignments = result.map(r => readAssignment[Response](r._2)).toList
      val hit = readHIT[Prompt](result.head._1)
      HITInfo(hit, assignments)
    }
  }

  /** Get all saved HIT data for a given HIT Type. */
  def getAllHITInfo[Prompt: Reader, Response : Reader](
    hitTypeId: String
  ): Try[List[HITInfo[Prompt, Response]]] = {
    val query =
      (hits join assignments on (_.hitId === _.hitId))
        .filter(_._1.hitTypeId === hitTypeId)
    val fut = db.run(query.result)
    Try {
      Await.result(fut, 2.minutes)
        .groupBy(_._1.hitId)
        .map { case (_, pairs) =>
        val assignments = pairs.map(r => readAssignment[Response](r._2)).toList
        val hit = readHIT[Prompt](pairs.head._1)
        HITInfo(hit, assignments)
      }.toList
    }

  }

  override def getAssignmentsForHIT[Response : Reader](
    hitTypeId: String,
    hitId: String
  ): Try[List[Assignment[Response]]] = {
    val query = assignments
      .filter(_.hitTypeId === hitTypeId)
      .filter(_.hitId === hitId)
    val fut = db.run(query.result)
    Try(Await.result(fut, 10.seconds).map(readAssignment[Response]).toList)
  }

}
