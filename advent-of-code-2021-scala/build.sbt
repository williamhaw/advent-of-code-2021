import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

val zioVersion = "2.0.0-RC1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2021-scala",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "dev.zio"                %% "zio"                      % zioVersion,
      "dev.zio"                %% "zio-test"                 % zioVersion % Test,
      "dev.zio"                %% "zio-test-sbt"             % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
