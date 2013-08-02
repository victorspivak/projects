import sbt._
import sbt.Keys._

object MdPocBuild extends Build {
    import com.github.retronym.SbtOneJar._
    import sbtbuildinfo.Plugin._

    val scalatraVersion = "2.0.4"

    resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"
    resolvers += "Thrift" at "http://people.apache.org/~rawson/repo/"
//    resolvers += "Maven" at "https://repository.sonatype.org/content/groups/forge/"
    resolvers += "Nexus" at "https://oss.sonatype.org/content/repositories/releases"

    resolvers += "amateras-repo" at "http://amateras.sourceforge.com/mvn/"


//    resolvers += (
//        "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository",
//        "Maven Repo" at "http://www.restlet.org/org.restlet.ext.servlet"
//        "Maven Repo" at "https://repository.sonatype.org/content/groups/forge/"
//    )

  lazy val deps = Seq(
    "org.slf4j" % "slf4j-api" % "1.7.5",
    "org.slf4j" % "slf4j-log4j12" % "1.7.5",
    "org.scalatra" %% "scalatra" % scalatraVersion,
    "org.scalatra" %% "scalatra-lift-json" % scalatraVersion,
//    "commons-logging" % "commons-logging" % "1.1.1",
//    "org.mongodb" % "casbah_2.9.2" % "2.5.1",
    "org.apache.hadoop"        %  "hadoop-core"          % "1.1.2",
    ("org.apache.hbase"         %  "hbase"                % "0.94.7").exclude("org.slf4j", "slf4j-log4j12"),
    ("org.apache.solr" % "solr-solrj" % "4.3.0").exclude("org.slf4j", "slf4j-simple"),
    ("svl"                      % "myscalalib_2.9.2"          %  "0.5.0").exclude("org.slf4j", "slf4j-simple"),
    "org.specs2"               % "specs2_2.9.2"         % "1.12.3" % "test",
    "org.hamcrest"             % "hamcrest-all"         % "1.1"    % "test",
    "org.mockito"              % "mockito-all"          % "1.9.0"  % "test",
    "junit"                    % "junit"                % "4.7"    % "test"
  )

  val ivyLocal = Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)


  val root = Project(
    id = "mdpoc",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        version := "1.54-SNAPSHOT",
        scalaVersion := "2.9.0-1",
        libraryDependencies ++= deps,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := "svl.metadata.poc",
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        initialCommands := """
                             |import svl.metadata.poc._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact("MdPocProject", "one-jar"), oneJar)
  )
}
