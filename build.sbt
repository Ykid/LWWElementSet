name := "GoodNotesChallenge"

version := "0.1"

scalaVersion := "2.13.0"

val scalaTestVersion = "3.0.8"

coverageExcludedPackages := ";.*proto.*;"


scalacOptions := Seq("-deprecation")

val compiledProtoPath = "src/main/scala-protos/scala"
unmanagedSourceDirectories in Compile += baseDirectory.value / compiledProtoPath


PB.targets in Compile := Seq(
  scalapb.gen(lenses = false) -> baseDirectory.value / compiledProtoPath
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
)

