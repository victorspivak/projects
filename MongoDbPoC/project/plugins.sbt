resolvers += "Box Dev Repo" at "http://maven.dev.box.net:8081/nexus/content/groups/public"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("net.box" %% "box-sbt" % "latest.release")

addSbtPlugin("com.github.retronym" %% "sbt-onejar" % "0.8")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.2.2")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.3.0-SNAPSHOT")



