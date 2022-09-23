ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "DER",
    version := "0.1",
    maintainer := "STH",
    scalaVersion := "2.13.8",
    organization := "org.Galois"
  )



//wartremoverErrors ++= Warts.unsafe

// https://mvnrepository.com/artifact/com.github.scopt/scopt
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.14.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

// https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-core
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.19.0"

// https://mvnrepository.com/artifact/org.apache.logging.log4j/log4j-api-scala
libraryDependencies += "org.apache.logging.log4j" %% "log4j-api-scala" % "12.0"

enablePlugins(sbtdocker.DockerPlugin, JavaAppPackaging)

docker / dockerfile := {
  val appDir: File = stage.value
  val targetDir = "/app"

  new Dockerfile {
    from("openjdk:8-jre")
    //Install Latex - takes a while to download
    runRaw("apt-get update && apt-get install -y --no-install-recommends apt-utils")
    runRaw("apt-get install texlive-full -y")
    runRaw("apt-get install -y cryptol")
    //Copy the application
    copy(appDir, targetDir, chown = "daemon:daemon")
    //Set the working directory
    workDir(s"$targetDir/bin/")
    entryPoint(s"$targetDir/bin/${executableScriptName.value}")
  }
}