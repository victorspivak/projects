resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.github.retronym" %% "sbt-onejar" % "0.8")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.2.2")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0-SNAPSHOT")
