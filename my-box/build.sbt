name := "my-box"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalacheck" % "scalacheck_2.10" % "1.10.1" % "test"
)     

play.Project.playScalaSettings
