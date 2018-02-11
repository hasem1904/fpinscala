name := "fpinscala"

version := "1.0"

scalaVersion := "2.12.1"

//connectInput in run := true

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12",
  "org.mockito" % "mockito-core" % "1.9.5",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

