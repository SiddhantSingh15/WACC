Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "compiler"

lazy val root = (project in file("."))
  .settings(
    name := projectName,
    organization := "uk.ac.imperial.doc",
    scalaVersion := "2.13.7",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.2"
  )
