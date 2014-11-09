organization := "com.typesafe.slick"

name := "LearnNetty"

version := "1.0.0"

scalaVersion := "2.11.0"

scalacOptions += "-deprecation"

// scala-compiler is declared as an optional dependency by Slick.
// You need to add it explicitly to your own project if you want
// to use the direct embedding
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies ++= List(
    "io.netty" % "netty-all" % "4.0.24.Final",
    "org.slf4j" % "slf4j-nop" % "1.6.4"
)

