ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "asn1scala",
    run / javaOptions ++= Seq(
      "-Xss1G"
    ),
    run / Keys.fork := true
  )
