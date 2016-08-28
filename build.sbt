name := "mturk-semantics"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.4",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "com.lihaoyi" %% "upickle" % "0.4.1",
  "com.typesafe.akka" %% "akka-actor" % "2.4.8",
  "com.lihaoyi" %% "scalatags" % "0.4.6",
  "com.jsuereth" % "scala-arm_2.11" % "2.0-RC1",
  "com.softwaremill.macmemo" %% "macros" % "0.4-SNAPSHOT",
  // java deps:
  "log4j" % "log4j" % "1.2.17",
  // only need this to escape strings for JS. won't be necessary after the switch to scala.js
  "org.apache.commons" % "commons-lang3" % "3.4",
  "net.sf.trove4j" % "trove4j" % "3.0.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  "net.ettinsmoor" % "java-aws-mturk" % "1.6.2"
    exclude("org.apache.commons","not-yet-commons-ssl")
    exclude("apache-xerces","xercesImpl")
    exclude("apache-xerces","resolver")
    exclude("apache-xerces","xml-apis"),
  "ca.juliusdavies" % "not-yet-commons-ssl" % "0.3.11",
  "xerces" % "xercesImpl" % "2.9.1"
)
