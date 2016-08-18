name := "mturk-semantics"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.4",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "com.lihaoyi" %% "upickle" % "0.2.7",
  "com.typesafe.akka" %% "akka-actor" % "2.4.8",
  "com.lihaoyi" %% "scalatags" % "0.4.6",
  // java deps:
  "log4j" % "log4j" % "1.2.17",
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
