ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file("."))
  .settings(
    name := "Advent-of-Code-2024",
    idePackagePrefix := Some("io.github.avapl"),
    libraryDependencies += "org.jline" % "jline" % "3.26.3"
  )
