name := "GoodNotesChallenge"

version := "0.1"

scalaVersion := "2.13.0"

val scalaTestVersion = "3.0.8"
libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"