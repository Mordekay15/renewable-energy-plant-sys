ThisBuild / scalaVersion := "2.13.14"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "reps"

lazy val root = (project in file("."))
  .settings(
    name := "renewable-energy-plant-sys",
    Compile / mainClass := Some("reps.Main"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xlint:_"
    )
  )