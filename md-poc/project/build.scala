import sbt._
import Keys._
import org.scalatra.sbt._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._

object MdPocBuild extends Build {
    import com.github.retronym.SbtOneJar._
    import sbtbuildinfo.Plugin._

    val MyOrganization = "svl"
    val MyProjectVersion = "0.5.0"
    val MyPackage = "svl.metadata.poc"
    val MyArtifact = "MdPocProject"

    val ScalaVersion = "2.10.2"
    val ScalatraVersion = "2.2.1"
    val Slf4jVersion = "1.7.5"
    val JUnitVersion = "4.7"
    val MochitoVersion = "1.9.0"
    val Specs2Version = "2.0"

    resolvers += "Apache HBase" at "https://repository.apache.org/content/repositories/releases"
    resolvers += "Thrift" at "http://people.apache.org/~rawson/repo/"
    resolvers += "Nexus" at "https://oss.sonatype.org/content/repositories/releases"
    resolvers += "amateras-repo" at "http://amateras.sourceforge.com/mvn/"

    lazy val deps = Seq(
      "org.slf4j" % "slf4j-api" % Slf4jVersion,
      "org.slf4j" % "slf4j-log4j12" % Slf4jVersion,
      "log4j" % "log4j" % "1.2.17",
      "org.scalatra" %% "scalatra" % ScalatraVersion,
      "org.scalatra" %% "scalatra-scalate" % ScalatraVersion,
      "org.scalatra" %% "scalatra-json" % ScalatraVersion,
      "org.json4s"   %% "json4s-jackson" % "3.2.4",

      "org.scalatra" %% "scalatra-specs2" % ScalatraVersion % "test",
      "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "compile,container",
      "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "compile,container;provided;test" artifacts Artifact("javax.servlet", "jar", "jar"),

      ("org.apache.hadoop"        %  "hadoop-core"          % "1.1.2")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      ("org.apache.hbase"         %  "hbase"                % "0.94.7")
//      ("org.apache.hbase"         %  "hbase-client"                % "0.98.3-hadoop1")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      ("org.apache.solr" % "solr-solrj" % "4.3.0")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      ("svl"                      %% "myscalalib"   %  "1.0.0")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      ("org.specs2" %% "specs2" % Specs2Version % "test")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      ("org.hamcrest"             % "hamcrest-all"         % "1.1"    % "test")
        .exclude("org.slf4j", "slf4j-log4j12")
        .exclude("org.slf4j", "slf4j-simple")
        .exclude("org.slf4j", "slf4j-api"),
      "org.mockito"              % "mockito-all"          % MochitoVersion  % "test",
      "junit"                    % "junit"                % JUnitVersion    % "test"
    )

  val ivyLocal = Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)

  lazy val root = Project(
    id = "mdpoc",
    base = file("."),
    settings =
      Defaults.defaultSettings ++
      ScalatraPlugin.scalatraWithJRebel ++
      scalateSettings ++
      oneJarSettings ++
      buildInfoSettings ++
      Seq(
        version := MyProjectVersion,
        scalaVersion := ScalaVersion,
        scalacOptions := Seq( "-deprecation", "-feature", "-unchecked" ),
        libraryDependencies ++= deps,
        scalateTemplateConfig in Compile <<= (sourceDirectory in Compile){ base =>
          Seq(
            TemplateConfig(
              base / "webapp" / "WEB-INF" / "templates",
              Seq.empty,  /* default imports should be added here */
              Seq(
                Binding("context", "_root_.org.scalatra.scalate.ScalatraRenderContext", importMembers = true, isImplicit = true)
              ),  /* add extra bindings here */
              Some("templates")
            )
          )
        },
        sourceGenerators in Compile <+= buildInfo,
        buildInfoPackage := MyPackage,
        connectInput in run := true,
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        publishArtifact in Test := false,

        fork in run := true,
        initialCommands := """
                             |import svl.metadata.poc._
                             |""".stripMargin
      ) ++
      addArtifact(Artifact(MyArtifact, "one-jar"), oneJar)
  )
}
