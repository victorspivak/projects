name := "ScalaBox"

version := "1.0"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory(_ / "src")

resolvers += Classpaths.typesafeResolver

resolvers += ScalaToolsReleases

libraryDependencies ~= { seq =>
  val vers = "0.8.9"
  seq ++ Seq(
    "net.databinder" %% "dispatch-core" % vers,
    "net.databinder" %% "dispatch-oauth" % vers,
    "net.databinder" %% "dispatch-nio" % vers,
    "net.databinder" %% "dispatch-http" % vers,
    "net.databinder" %% "dispatch-mime" % vers,
    "net.databinder" %% "dispatch-tagsoup" % vers,
    "net.databinder" %% "dispatch-jsoup" % vers
  )
}

initialCommands := "import dispatch._"
