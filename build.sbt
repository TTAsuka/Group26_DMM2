ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "DMM2",
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.9.2", // JSON解析库
      "org.sameersingh.scalaplot" %% "scalaplot" % "0.0.4" // Scala图表库
    )
  )
