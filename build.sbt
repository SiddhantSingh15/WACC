Global / onChangedBuildSource := ReloadOnSourceChanges

val projectName = "compiler"

lazy val sbtAssemblySettings = baseAssemblySettings ++ Seq(
  assembly / assemblyOutputPath := baseDirectory.value / s"$projectName.jar",
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", xs @ _*) => MergeStrategy.discard
    case _ => MergeStrategy.first
  }
)

lazy val root = (project in file("."))
  .settings(
    name := projectName,
    organization := "uk.ac.imperial.doc",
    scalaVersion := "2.13.7",
    version := "0.1.0",
    sbtAssemblySettings,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % Test,
    libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.2"
  )


