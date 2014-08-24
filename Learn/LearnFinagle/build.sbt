organization  := "svl"

name := "Hello Finagle"

version       := "0.1"

scalaVersion  := "2.10.3"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val finagleV    = "6.20.0"
  val spec2V      = "2.3.7"
  val json4sV     = "3.2.8"
  Seq(
    "com.twitter"           %%  "finagle-http"      % finagleV,
    "com.twitter"           %%  "finagle-core"      % finagleV,
    "org.json4s"            %%  "json4s-native"     % json4sV,
    "org.specs2"            %%  "specs2-core"       % spec2V % "test"
  )
}

