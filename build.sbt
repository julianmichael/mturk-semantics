val scalaJSReactVersion = "1.1.0"
val monocleVersion = "1.4.0-M2"

lazy val root = project.in(file("."))
  .aggregate(turksemJVM, turksemJS,
             emnlp2017JVM, emnlp2017JS,
             ai2JVM, ai2JS,
             multitaskJVM, multitaskJS,
             tqaJVM, tqaJS,
             tqaevalJVM, tqaevalJS,
             interactiveJVM, interactiveJS,
             framerJVM, framerJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val commonSettings = Seq(
  organization := "com.github.julianmichael",
  scalaOrganization in ThisBuild := "org.typelevel",
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"/*, "-Ypartial-unification"*/),
  // addCompilerPlugin("io.tryp" %% "splain" % "0.2.7"), // TODO wait until this gets fixed
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies += "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
  libraryDependencies += "com.github.julianmichael" %%% "spacro" % "0.1-SNAPSHOT",
  libraryDependencies += "com.github.uwnlp" %%% "qamr" % "0.1-SNAPSHOT",
  libraryDependencies += "com.github.julianmichael" %%% "qasrl" % "0.1-SNAPSHOT",
  libraryDependencies += "com.github.julianmichael" %%% "qasrl-crowd" % "0.1-SNAPSHOT",
  libraryDependencies += "org.typelevel" %% "cats" % "0.9.0",
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "0.3",
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.11.0",
  libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.3",
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion,
  libraryDependencies += "com.softwaremill.macmemo" %% "macros" % "0.4-SNAPSHOT"
)

lazy val commonJVMSettings = Seq(
  fork in console := true,
  connectInput in run := true,
  libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
  libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models" // for automatically downloading pos-tagging model
)

// TODO: update this to the new hotness with newer scala.js version!
// scalaJSUseMainModuleInitializer
// need to change this in spacro as well..

lazy val commonJSSettings = Seq(
  relativeSourceMaps := true,
  scalaJSStage in Global := FastOptStage,
  persistLauncher in Compile := true,
  persistLauncher in Test := false,
  skip in packageJSDependencies := false)

lazy val turksem = crossProject
  .settings(commonSettings).settings(
  name := "turksem",
  version := "0.1-SNAPSHOT"
).jvmSettings(commonJVMSettings).jvmSettings(
  fork in console := true,
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    "com.typesafe.slick" %% "slick" % "3.2.1",
    "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    // java deps:
    "org.slf4j" % "slf4j-api" % "1.7.21" // decided to match scala-logging transitive dep
  )
).jsSettings(commonJSSettings).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-cats" % scalaJSReactVersion,
    "com.github.japgolly.scalacss" %%% "core" % "0.5.3",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3"
  ),
  jsDependencies ++= Seq(
    RuntimeDOM,
    "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js",

    "org.webjars.bower" % "react" % "15.6.1"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.6.1"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % "15.6.1"
      /         "react-dom-server.js"
      minified  "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
  )
)

lazy val turksemJS = turksem.js
lazy val turksemJVM = turksem.jvm

lazy val exampleProjectSettings = commonSettings ++ Seq(
)

lazy val exampleProjectJVMSettings = commonJVMSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "scalatags" % "0.6.5",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    // java deps:
    "org.slf4j" % "slf4j-api" % "1.7.21", // decided to match scala-logging transitive dep
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "log4j" % "log4j" % "1.2.17", // runtime error if not included?
    "ca.juliusdavies" % "not-yet-commons-ssl" % "0.3.11",  // runtime error if not included?
    "xerces" % "xercesImpl" % "2.9.1" // runtime error if not included?
  )
)

lazy val exampleProjectJSSettings = commonJSSettings

lazy val emnlp2017 = crossProject.in(file("example/emnlp2017")).settings(
  name := "turksem-emnlp2017",
  version := "0.1-SNAPSHOT"
).settings(
  exampleProjectSettings
).settings(
  libraryDependencies += "com.github.uwnlp" %%% "qamr-example" % "0.1-SNAPSHOT",
  libraryDependencies += "com.github.uwnlp" %%% "qamr-analysis" % "0.1-SNAPSHOT"
).jvmSettings(
  exampleProjectJVMSettings
).jvmSettings(
  libraryDependencies += "io.argonaut" %% "argonaut" % "6.1"
).jsSettings(
  exampleProjectJSSettings
)

lazy val emnlp2017JS = emnlp2017.js.dependsOn(turksemJS)
lazy val emnlp2017JVM = emnlp2017.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (emnlp2017JS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (emnlp2017JS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (emnlp2017JS, Compile)).value
)

lazy val ai2 = crossProject.in(file("example/ai2"))
  .settings(name := "turksem-ai2", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val ai2JS = ai2.js.dependsOn(turksemJS)
lazy val ai2JVM = ai2.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (ai2JS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (ai2JS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (ai2JS, Compile)).value
)

lazy val multitask = crossProject.in(file("example/multitask"))
  .settings(name := "turksem-multitask", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val multitaskJS = multitask.js.dependsOn(turksemJS)
lazy val multitaskJVM = multitask.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (multitaskJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (multitaskJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (multitaskJS, Compile)).value
)

lazy val tqa = crossProject.in(file("example/tqa"))
  .settings(name := "turksem-tqa", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val tqaJS = tqa.js.dependsOn(turksemJS)
lazy val tqaJVM = tqa.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (tqaJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (tqaJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (tqaJS, Compile)).value
)

lazy val tqaeval = crossProject.in(file("example/tqaeval"))
  .settings(name := "turksem-tqaeval", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val tqaevalJS  = tqaeval.js.dependsOn(turksemJS)
lazy val tqaevalJVM = tqaeval.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (tqaevalJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (tqaevalJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (tqaevalJS, Compile)).value
)

lazy val interactive = crossProject.in(file("example/interactive"))
  .settings(name := "turksem-interactive", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val interactiveJS = interactive.js.dependsOn(turksemJS, emnlp2017JS)
lazy val interactiveJVM = interactive.jvm.dependsOn(turksemJVM, emnlp2017JVM).settings(
  (resources in Compile) += (fastOptJS in (interactiveJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (interactiveJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (interactiveJS, Compile)).value
)

lazy val framer = crossProject.in(file("example/framer"))
  .settings(name := "turksem-framer", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val framerJS = framer.js.dependsOn(turksemJS)
lazy val framerJVM = framer.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (framerJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (framerJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (framerJS, Compile)).value
)

import sbtassembly.AssemblyPlugin.defaultShellScript

lazy val qtrans = project.in(file("example/qtrans"))
  .dependsOn(turksemJVM)
  .settings(
  organization := "com.github.julianmichael",
  name := "turksem-qtrans",
  version := "0.1-SNAPSHOT",
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies += "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
  libraryDependencies += "org.typelevel" %% "cats" % "0.9.0",
  libraryDependencies += "com.monovore" %% "decline" % "0.3.0"
).settings(
  // assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript)),
  assemblyJarName in assembly := "qtrans.jar",
  test in assembly := {}, // not like I have tests anyway, ha!
  assemblyMergeStrategy in assembly := { // TODO this is a hack basically to get TL and LB scala to play nice
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case x => MergeStrategy.last
  }
)

lazy val extra = project.in(file("example/extra"))
  .settings(
  organization := "com.github.julianmichael",
  name := "turksem-extra",
  version := "0.1-SNAPSHOT",
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds", "-Ypartial-unification"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.1",
  libraryDependencies += "co.fs2" %% "fs2-io" % "0.10.1",
  libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.18.3"
  // libraryDependencies += "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
  // libraryDependencies += "org.typelevel" %% "cats" % "1.0.0",
  // libraryDependencies += "com.monovore" %% "decline" % "0.3.0"
)


lazy val facty = crossProject.in(file("example/facty"))
  .settings(name := "turksem-facty", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

// lazy val factyJS =  facty.js.dependsOn(turksemJS)
lazy val factyJVM = facty.jvm.dependsOn(turksemJVM)

lazy val qacmp = crossProject.in(file("example/qacmp"))
  .settings(name := "turksem-qacmp", version := "0.1-SNAPSHOT")
  .settings(exampleProjectSettings)
  .jvmSettings(exampleProjectJVMSettings)
  .jsSettings(exampleProjectJSSettings)

lazy val qacmpJS = qacmp.js.dependsOn(turksemJS)
lazy val qacmpJVM = qacmp.jvm.dependsOn(turksemJVM).settings(
  (resources in Compile) += (fastOptJS in (qacmpJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (qacmpJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (qacmpJS, Compile)).value
)
