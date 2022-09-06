ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "DER"
  )

//wartremoverErrors ++= Warts.unsafe


// https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-core
//libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.18.0"

// https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-api-scala
//libraryDependencies += "org.apache.logging.log4j" %% "log4j-api-scala" % "12.0"

// https://mvnrepository.com/artifact/com.github.scopt/scopt
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

// From https://alvinalexander.com/scala/how-use-twirl-templates-standalone-play-framework/
//lazy val root = (project in file(".")).enablePlugins(SbtTwirl)