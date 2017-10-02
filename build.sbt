name := "scala-basics"

version := "1.0.0"

scalaVersion in ThisBuild := "2.11.8"

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature")

lazy val commons = project.settings(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "junit" % "junit" % "4.12" % "test"
  )
)

lazy val example = project.dependsOn(commons % "compile->compile;test->test")

lazy val functions = project.dependsOn(commons % "compile->compile;test->test")

lazy val classes =
  project.dependsOn(commons % "compile->compile;test->test")
    .settings(libraryDependencies += "org.json4s" %% "json4s-native" % "3.4.1")

lazy val patmat = project.dependsOn(commons % "compile->compile;test->test")

lazy val lists = project.dependsOn(commons % "compile->compile;test->test")

lazy val forcomp = project.dependsOn(commons % "compile->compile;test->test")

lazy val streams = project.dependsOn(commons % "compile->compile;test->test")

lazy val root = project.in(file(".")).aggregate(
  example,
  functions,
  classes,
  patmat,
  lists,
  forcomp,
  streams
)
