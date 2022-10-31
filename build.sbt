ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "der",
    version := "0.1.5",
    maintainer := "STH",
    scalaVersion := "2.13.8",
    organization := "org.Galois"
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

// JSON library
val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

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
    // install -y libtinfo5
    runRaw("apt-get install -y libtinfo5")

    // Install CMAKE
    runRaw("apt-get install -y cmake make")

    runRaw("mkdir /tools")
    //Install Bluespec Compiler
    workDir("/tmp")
    runRaw("wget https://github.com/B-Lang-org/bsc/releases/download/2021.07/bsc-2021.07-ubuntu-20.04.tar.gz")
    runRaw("tar -xvf bsc-2021.07-ubuntu-20.04.tar.gz")
    runRaw("mv bsc-2021.07-ubuntu-20.04 /tools/bsc-2021.07-ubuntu-20.04")
    runRaw("rm bsc-2021.07-ubuntu-20.04.tar.gz")
    env("PATH", "/tools/bsc-2021.07-ubuntu-20.04/bin:$PATH")

    //Clone the Bluespec Compiler
    runRaw("git clone https://github.com/B-Lang-org/bsc-contrib.git /tools/bsc-contrib")
    workDir("/tools/bsc-contrib")
    runRaw("git checkout aa205330885f6955e24fd99a0319e2733b5353f1")
    runRaw("make PREFIX=/tools/bsc-2021.07-ubuntu-20.04")

    //Install Yosys - not sure if this is needed
    //runRaw("apt-get install -y yosys")

    //Install SAW Script
    runRaw("wget https://github.com/GaloisInc/saw-script/releases/download/v0.9/saw-0.9-Linux-x86_64-with-solvers.tar.gz")
    runRaw("tar -xvf saw-0.9-Linux-x86_64-with-solvers.tar.gz")
    runRaw("mv saw-0.9-Linux-x86_64-with-solvers /tools/saw-0.9-Linux-x86_64-with-solvers")
    runRaw("rm saw-0.9-Linux-x86_64-with-solvers.tar.gz")
    env("PATH", "/tools/saw-0.9-Linux-x86_64-with-solvers/bin:$PATH")

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