ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "AOC2023"
  )
