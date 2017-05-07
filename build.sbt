val monocleVersion = "1.4.0-M2"
val scalaJSReactVersion = "0.11.1"

lazy val root = project.in(file("."))
  .aggregate(mtsJVM, mtsJS, nlpdataJVM, nlpdataJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val nlpdata = crossProject.settings(
  name := "nlpdata",
  organization := "org.me", // TODO com.github.uwnlp?
  version := "0.1-SNAPSHOT",
  scalaOrganization in ThisBuild := "org.typelevel", // for fixing stupid serialization woes
  scalaVersion in ThisBuild := "2.11.8", // TODO cross-build
  scalacOptions ++= Seq("-language:higherKinds", "-deprecation", "-feature", "-unchecked"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies += "org.typelevel" %%% "cats" % "0.9.0",
  libraryDependencies += "com.softwaremill.macmemo" %% "macros" % "0.4-SNAPSHOT",
  libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.3.7"
).jvmSettings(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing(),
  libraryDependencies += "com.jsuereth" % "scala-arm_2.11" % "2.0-RC1",
  libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.1",
  // java
  libraryDependencies += "net.sf.trove4j" % "trove4j" % "3.0.1"
)

lazy val mts = crossProject.settings(
  name := "mts",
  organization := "org.me", // TODO: com.github.uwnlp?
  version := "0.1-SNAPSHOT",
  scalaOrganization in ThisBuild := "org.typelevel", // for fixing stupid serialization woes
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "upickle" % "0.4.1",
    "com.lihaoyi" %%% "scalatags" % "0.4.6",
    "com.lihaoyi" %%% "autowire" % "0.2.5",
    "com.lihaoyi" %%% "fastparse" % "0.3.7",
    "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion
  )
).jvmSettings(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  fork in console := true,
  javaOptions in console += "-Djava.library.path=lib/native/",
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.4",
    // TODO eventually switch to this if necessary
    // "com.github.pathikrit" % "better-files_2.11" % "2.16.0",
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
    "com.jsuereth" % "scala-arm_2.11" % "2.0-RC1",
    "com.softwaremill.macmemo" %% "macros" % "0.4-SNAPSHOT",
    "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing(),
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
).jsSettings(
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % scalaJSReactVersion,
    "com.github.japgolly.scalacss" %%% "core" % "0.4.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1"
    // "com.github.julien-truffaut" %%% "monocle-law"   % monocleVersion % "test"
  ),
  relativeSourceMaps := true,
  scalaJSStage in Global := FastOptStage,
  persistLauncher in Compile := true,
  persistLauncher in Test := false,
  skip in packageJSDependencies := false,
  jsDependencies ++= Seq(
    RuntimeDOM,
    "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js",

    "org.webjars.bower" % "react" % "15.0.2"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.0.2"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % "15.0.2"
      /         "react-dom-server.js"
      minified  "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
  )
)

lazy val nlpdataJS = nlpdata.js
lazy val nlpdataJVM = nlpdata.jvm.settings(
  (resources in Compile) += (fastOptJS in (nlpdataJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (nlpdataJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (nlpdataJS, Compile)).value
)

lazy val mtsJS = mts.js.dependsOn(nlpdataJS)
lazy val mtsJVM = mts.jvm.dependsOn(nlpdataJVM).settings(
  (resources in Compile) += (fastOptJS in (mtsJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (mtsJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (mtsJS, Compile)).value
)
