ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.11"

lazy val root = (project in file("."))
  .settings(
    name := "DMM2",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.9.2",
      "org.scalanlp" %% "breeze-viz" % "1.2"
    )
  )
