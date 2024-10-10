ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

resolvers += "Sonatype" at "https://s01.oss.sonatype.org/"

enablePlugins(Antlr4Plugin)
Antlr4 / antlr4Version := "4.11.1"
Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4GenListener := false
Antlr4 / antlr4Dependency := "org.antlr" % "antlr4" % "4.11.1"
Antlr4 / antlr4RuntimeDependency :=  "org.antlr" % "antlr4" % "4.11.1"
Antlr4 / antlr4Generate := Seq(file("nl/njtromp/adventofcode_2018/Day12_2018.g4"))
Antlr4 / antlr4PackageName := Some("nl.njtromp")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC5"

libraryDependencies += "org.antlr" % "antlr4" % "4.11.1"
libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.11.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

libraryDependencies += "org.junit.jupiter" % "junit-jupiter" % "5.9.0"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode"
  )
