ThisBuild / version := "0.25.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
//libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.14.6206"
libraryDependencies ++=  Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % "0.14.15")

lazy val root = (project in file("."))
  .settings(
    name := "AOC2025"
  )
