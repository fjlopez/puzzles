import Dependencies._

name := "stacking-the-deck"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "es.unizar.fjlopez",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test
  )

libraryDependencies ++= Seq(
	"com.typesafe.scala-logging" % "scala-logging_2.12" % "3.5.0",
    "org.slf4j" % "slf4j-simple" % "1.7.21"
)


