import sbt._
import sbt.Keys._

object LearnScalazBuild extends Build {
  import com.github.retronym.SbtOneJar._
  import sbtbuildinfo.Plugin._

  lazy val deps = Seq(
    "commons-logging" % "commons-logging" % "1.1.1",
    "org.mockito"              % "mockito-all"          % "1.9.0"  % "test",
    "org.scalaz" % "scalaz-core_2.10" % "7.0.6",
    "org.specs2" % "specs2_2.10" % "2.3.11",
    "junit"                    % "junit"                % "4.7"    % "test"
  )

  val root = Project(
    id = "learnscalaz",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        version := "1.54-SNAPSHOT",
        scalaVersion := "2.10.2",
        libraryDependencies ++= deps,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := "svl.scalaz",
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        javaOptions ++= Seq("-Dbox.environments=devvm," + System.getProperty("user.name"), "-Dbox.service.id=1"),
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
        initialCommands := """
                             |import svl.scalaz._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact("ScalaZ", "one-jar"), oneJar)
  )
}
