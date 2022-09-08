ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "DER"
  )

//wartremoverErrors ++= Warts.unsafe


// https://mvnrepository.com/artifact/com.github.scopt/scopt
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

libraryDependencies ++= Seq(
  "org.legogroup" %% "woof-core" % "0.4.5",
  "org.legogroup" %% "woof-slf4j" % "0.4.5",
)
