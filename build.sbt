val monocleVersion = "1.4.0-M2"
val scalaJSReactVersion = "0.11.1"

lazy val root = project.in(file("."))
  .aggregate(qamrJVM, qamrJS, emnlp2017JVM, emnlp2017JS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val commonSettings = Seq(
  organization := "com.github.julianmichael",
  scalaOrganization in ThisBuild := "org.typelevel", // for fixing stupid serialization woes
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"),
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val commonJVMSettings = Seq(
  fork in console := true
)

lazy val commonJSSettings = Seq(
  relativeSourceMaps := true,
  scalaJSStage in Global := FastOptStage,
  persistLauncher in Compile := true,
  persistLauncher in Test := false,
  skip in packageJSDependencies := false)

lazy val qamr = crossProject
  .settings(commonSettings).settings(
  name := "qamr",
  version := "0.1-SNAPSHOT",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies ++= Seq(
    "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
    "com.github.julianmichael" %%% "turkey" % "0.1-SNAPSHOT",
    "com.lihaoyi" %%% "upickle" % "0.4.1"
  )
).jvmSettings(commonJVMSettings).jvmSettings(
  fork in console := true,
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    // java deps:
    "org.slf4j" % "slf4j-api" % "1.7.21" // decided to match scala-logging transitive dep
  )
).jsSettings(commonJSSettings).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % scalaJSReactVersion,
    "com.github.japgolly.scalacss" %%% "core" % "0.4.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1",
    "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion
  ),
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

lazy val qamrJS = qamr.js
lazy val qamrJVM = qamr.jvm

lazy val exampleProjectSettings = Seq(
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies ++= Seq(
    "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
    "com.github.julianmichael" %%% "turkey" % "0.1-SNAPSHOT",
    "org.typelevel" %% "cats" % "0.9.0",
    "com.lihaoyi" %%% "upickle" % "0.4.1"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)

lazy val exampleProjectJVMSettings = Seq(
  libraryDependencies ++= Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    // java deps:
    "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
    "org.slf4j" % "slf4j-api" % "1.7.21", // decided to match scala-logging transitive dep
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
)

lazy val emnlp2017 = crossProject.in(file("example/emnlp2017"))
  .settings(commonSettings)
  .settings(exampleProjectSettings)
  .settings(name := "qamr-emnlp2017",
            version := "0.1-SNAPSHOT")
  .jvmSettings(commonJVMSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jvmSettings(
  libraryDependencies += "io.argonaut" %% "argonaut" % "6.1"
)
  .jsSettings(commonJSSettings)

lazy val emnlp2017JS = emnlp2017.js.dependsOn(qamrJS)
lazy val emnlp2017JVM = emnlp2017.jvm.dependsOn(qamrJVM).settings(
  (resources in Compile) += (fastOptJS in (emnlp2017JS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (emnlp2017JS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (emnlp2017JS, Compile)).value
)
