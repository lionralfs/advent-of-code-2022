ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "com.example"

lazy val hello = (project in file("."))
  .settings(
    name := "advent-of-code-2022",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test
  )