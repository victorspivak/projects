import sbt._
import sbt.Keys._

object LearnScalazBuild extends Build {
  import com.github.retronym.SbtOneJar._
  import sbtbuildinfo.Plugin._

  lazy val deps = Seq(
    // NOTE: We use "latest.integration", which is the most recent SNAPSHOT version, to try to catch box-common bugs earlier!
    // NOTE: There is another dep: sudo cp ~/libjpam.so /usr/java/jdk1.6.0_26/jre/lib/amd64
//    "org.slf4j" % "slf4j-simple" % "1.6.4",
    "commons-logging" % "commons-logging" % "1.1.1",
    "org.mockito"              % "mockito-all"          % "1.9.0"  % "test",
//    "org.scalaz"              %% "scalaz-core"          % "7.0.0",
//    "org.specs2"               % "specs2_2.9.2"         % "1.12.3" % "test",
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "org.specs2" %% "specs2" % "1.8.2" % "test",

    "junit"                    % "junit"                % "4.7"    % "test"
  )

  val root = Project(
    id = "learnscalaz",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
//      boxSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        version := "1.54-SNAPSHOT",
//        scalaVersion := "2.9.2",
        scalaVersion := "2.9.1",
        libraryDependencies ++= deps,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := "svl.scalaz",
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        javaOptions ++= Seq("-Dbox.environments=devvm," + System.getProperty("user.name"), "-Dbox.service.id=1"),
        initialCommands := """
                             |import svl.scalaz._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact("ScalaZ", "one-jar"), oneJar)
  )
}
