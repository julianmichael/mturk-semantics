package turksem.util

import cats.implicits._

// has nothing to do with the project really

import java.nio.charset._
import java.nio.file._
import java.io.File

case class SensePairInstance(
  propBankFramesetId: String,
  verbNetClass: String)

object PropBankLinker {

  // TODO use streams
  def argInstancesIterator = {
    import scala.collection.JavaConverters._
    for {
      section <- new File("resources/semlink-1.2.2c/wsjTokens").listFiles.iterator
      file <- section.listFiles.iterator
      line <- Files.readAllLines(Paths.get(file.getPath), Charset.forName("UTF-8")).asScala.iterator
      arr = line.split(" ")
    } yield SensePairInstance(
      propBankFramesetId = arr(7),
      verbNetClass = arr(5)
    )
  }

  lazy val sensePairMappings = {
    val fileString = scala.io.Source.fromFile("resources/semlink-1.2.2c/vn-pb/vnpbMappings.txt").getLines.mkString("\n")
    val xmlFile = scala.xml.XML.loadString(fileString)

    (xmlFile \ "predicate" \ "argmap").toList.map(node =>
      SensePairInstance(
        propBankFramesetId = (node \ "@pb-roleset").text,
        verbNetClass = (node \ "@vn-class").text
      ) -> (node \ "role").toList.map(roleNode =>
        ("ARG" + (roleNode \ "@pb-arg").text) -> (roleNode \ "@vn-theta").text
      ).toMap
    ).toMap
  }

  lazy val instancesByPropBankSense = argInstancesIterator.toList
    .groupBy(_.propBankFramesetId)
    .map { case (pbId, instances) =>
      pbId -> counts(instances.map(_.verbNetClass))
  }

  lazy val roleMappingsByPropBankSense = instancesByPropBankSense
    .map { case (pbFramesetId, instanceCounts) =>
      pbFramesetId -> instanceCounts.toVector.sortBy(-_._2).map(_._1)
        .foldLeft(Map.empty[String, String]) { case (acc, vnClass) =>
          sensePairMappings.get(SensePairInstance(pbFramesetId, vnClass)).fold(acc)(roleMapping =>
            roleMapping.foldLeft(acc) { case (acc2, (pbRole, vnRole)) =>
              if(acc2.contains(pbRole)) acc2 else acc2.updated(pbRole, vnRole)
            }
          )
      }
  }

  def writeRoleMappingsToFile(filename: String) = {
    val str = roleMappingsByPropBankSense.map { case (pbFramesetId, roleMapping) =>
      roleMapping.map { case (pbRole, vnRole) =>
        s"$pbFramesetId\t$pbRole\t$vnRole"
      }.mkString("\n")
    }.mkString("\n")
    Files.write(Paths.get(filename), str.getBytes(StandardCharsets.UTF_8))
  }
}






