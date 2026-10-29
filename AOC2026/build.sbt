ThisBuild / version := "0.26.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.10.0"

// Compiler like support
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.5.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.3.0"
// Linear algebra
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.14.6206"
// JSON processing
libraryDependencies ++=  Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.14.15")
// HTTP-client
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.13"

lazy val root = (project in file("."))
  .settings(
    name := "AOC2026"
  )
