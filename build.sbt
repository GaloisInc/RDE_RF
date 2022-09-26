ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.13"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "der",
    version := "0.1",
    maintainer := "STH",
    scalaVersion := "2.13.8",
    organization := "org.Galois",
  )



//wartremoverErrors ++= Warts.unsafe

// https://mvnrepository.com/artifact/com.github.scopt/scopt
libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"

// The version is chosen on purpose to be compatible with the version of Scala used in the project.
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
    //Install Cabal
    runRaw("apt-get install -y cabal-install")
    //Install Haskell Stack
    runRaw("cabal update")
    //Install Clang
    runRaw("apt-get install -y clang")
    // Install  apt-get install zlib1g-dev
    runRaw("apt-get install -y zlib1g-dev")
    //Install git
    runRaw("apt-get install -y git")

    //Install

    //Clone the SAW repository
    runRaw("git clone https://github.com/GaloisInc/saw-script.git /tmp/saw")
    //Build SAW
    workDir("/tmp/saw")
    runRaw("git checkout e2fef66d7cf4c67ecb86b0fe096977cd7e925183")
    runRaw("./build.sh")
    //Copy the SAW into path
    runRaw("cp /tmp/saw/bin/saw /usr/local/bin/saw")


    //Clone the Lando repo
    runRaw("git clone https://github.com/GaloisInc/BESSPIN-Lando.git /tools/lando")
    //Build Lando
    workDir("/tools/lando")
    runRaw("git checkout 428ea1174de2bed7c069a6ef8edb30ca75ed441a")
    runRaw("apt-get install -y maven")
    workDir("/tools/lando")
    //runRaw("./lando.sh -r")
    //Build Lobot
    workDir("/tools/lando/source/lobot")
    runRaw("cabal v2-build")
    //Copy the Lobot into path
    runRaw("find $(pwd) -type f -name lobot -exec cp {} /usr/local/bin/lobot \\;")
    env("PATH", s"tools/lando:$${PATH}")

    //Copy the application
    copy(appDir, targetDir, chown = "daemon:daemon")
    //Set the working directory
    workDir(s"$targetDir/bin/")
    entryPoint(s"$targetDir/bin/${executableScriptName.value}")

  }

  //Alternative Dockerfile
  /*
  new Dockerfile {
    from("HardensImage:latest")
    //Install Latex - takes a while to download
    runRaw("apt-get update && apt-get install -y --no-install-recommends apt-utils")
    runRaw("apt-get install texlive-full -y")

    //Copy the application
    copy(appDir, targetDir, chown = "daemon:daemon")
    //Set the working directory
    workDir(s"$targetDir/bin/")
    //entryPoint(s"$targetDir/bin/${executableScriptName.value}")
  )
   */
}

docker / imageNames := Seq(
  // Sets the latest tag
  ImageName(s"${organization.value}/${name.value}:latest"),

  // Sets a name with a tag that contains the project version
  ImageName(
    namespace = Some(organization.value),
    repository = name.value,
    tag = Some("v" + version.value)
  )
)