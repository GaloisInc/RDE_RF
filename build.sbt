ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"


lazy val root = (project in file("."))
  .settings(
    name := "DER"
  )


