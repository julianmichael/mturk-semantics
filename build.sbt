val monocleVersion = "1.4.0-M2"
val scalaJSReactVersion = "0.11.1"

lazy val root = project.in(file("."))
  .aggregate(mtsJVM, mtsJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val mts = crossProject.settings(
  name := "mts",
  organization := "com.github.julianmichael",
  version := "0.1-SNAPSHOT",
  scalaOrganization in ThisBuild := "org.typelevel", // for fixing stupid serialization woes
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies ++= Seq(
    "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
    "com.github.julianmichael" %%% "turkey" % "0.1-SNAPSHOT",
    "com.lihaoyi" %%% "upickle" % "0.4.1",
    "com.lihaoyi" %%% "scalatags" % "0.4.6",
    "com.lihaoyi" %%% "fastparse" % "0.3.7",
    "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion
  ),
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
).jvmSettings(
  fork in console := true,
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.4", // TODO get rid of this
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    // java deps:
    "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
    "org.slf4j" % "slf4j-api" % "1.7.21", // decided to match scala-logging transitive dep
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % scalaJSReactVersion,
    "com.github.japgolly.scalacss" %%% "core" % "0.4.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1"
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

lazy val mtsJS = mts.js
lazy val mtsJVM = mts.jvm.settings(
  (resources in Compile) += (fastOptJS in (mtsJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (mtsJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (mtsJS, Compile)).value
)
