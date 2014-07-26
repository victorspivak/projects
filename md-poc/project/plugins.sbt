resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.github.retronym" %% "sbt-onejar" % "0.8")

addSbtPlugin("com.mojolly.scalate" % "xsbt-scalate-generator" % "0.4.2")

addSbtPlugin("org.scalatra.sbt" % "scalatra-sbt" % "0.3.5")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")

