ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val root = (project in file("."))
  .settings(
    name := "AOC2024"
  )
