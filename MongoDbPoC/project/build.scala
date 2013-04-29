import sbt._
import sbt.Keys._

object MongoPocBuild extends Build {
  import net.box.sbt.BoxPlugin._
  import com.github.retronym.SbtOneJar._
  import sbtbuildinfo.Plugin._

  val scalatraVersion = "2.0.4"

  lazy val deps = Seq(
    // NOTE: We use "latest.integration", which is the most recent SNAPSHOT version, to try to catch box-common bugs earlier!
    // NOTE: There is another dep: sudo cp ~/libjpam.so /usr/java/jdk1.6.0_26/jre/lib/amd64
//    "org.mongodb" %% "casbah" % "3.0.0-M2",
    "org.mongodb" % "casbah_2.9.2" % "2.5.1",
//    "org.slf4j" % "slf4j-simple" % "1.6.4",
    "net.box" %% "box-common" % "latest.integration",
    "org.scalatra" %% "scalatra" % scalatraVersion,
    "org.scalatra" %% "scalatra-lift-json" % scalatraVersion,
    "commons-logging" % "commons-logging" % "1.1.1",
    "svl"                      % "myscalalib_2.9.2"          %  "0.5.0",
    "org.specs2"               % "specs2_2.9.2"         % "1.12.3" % "test",
    "org.hamcrest"             % "hamcrest-all"         % "1.1"    % "test",
    "org.mockito"              % "mockito-all"          % "1.9.0"  % "test",
    "junit"                    % "junit"                % "4.7"    % "test"
  )

  val ivyLocal = Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)


  val root = Project(
    id = "mongopoc",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
      boxSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        version := "1.54-SNAPSHOT",
        scalaVersion := "2.9.0-1",
        libraryDependencies ++= deps,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := "svl.mongo",
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        javaOptions ++= Seq("-Dbox.environments=devvm," + System.getProperty("user.name"), "-Dbox.service.id=1"),
        initialCommands := """
                             |import svl.mongo._
                             |import net.box.util.service._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact("MongoPoC", "one-jar"), oneJar)
  )
}
