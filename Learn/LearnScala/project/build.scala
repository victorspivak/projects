import sbt._
import sbt.Keys._

object MyScalaLibBuild extends Build {
  import com.github.retronym.SbtOneJar._
  import sbtbuildinfo.Plugin._

  val MyOrganization = "svl"
  val MyArtifact = "MyScalaLib"
  val MyPackage = "svl.scala.learn"
  val MyProjectVersion = "1.0.0"

  val ScalaVersion = "2.10.2"
  val ScalaVersionDep = "2.10"
  val AkkaVersion = "2.2.3"

  val ScalatraVersion = "2.2.1"
  val Slf4jVersion = "1.7.5"
  val JUnitVersion = "4.7"
  val MochitoVersion = "1.9.0"
  val Specs2Version = "2.0"

  lazy val deps = Seq(
    "com.typesafe.akka" %%  "akka-actor" % AkkaVersion,
    "com.typesafe.akka" %% "akka-remote" % AkkaVersion,
    "org.scala-lang" % "scala-reflect" % ScalaVersion,
    "com.novocode" % "junit-interface" % "0.10" % "test",
    "org.scala-lang" % "scala-swing" % ScalaVersion,

    "org.json4s" %% "json4s-native" % "3.2.8",

    "org.slf4j" % "slf4j-simple" % Slf4jVersion,
    "commons-logging" % "commons-logging" % "1.1.1",
    "org.specs2" % ("specs2_" + ScalaVersionDep) % Specs2Version % "test",
    "org.mockito"              % "mockito-all"          % MochitoVersion  % "test",
    "junit"                    % "junit"                % JUnitVersion    % "test"
  )

  val root = Project(
    id = "learnScala",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        organization := MyOrganization,
        version := MyProjectVersion,
        scalaVersion := ScalaVersion,
        libraryDependencies ++= deps,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := MyPackage,
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
        initialCommands := """
                             |import svl.scala.learn._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact(MyArtifact, "one-jar"), oneJar)
  )
}
