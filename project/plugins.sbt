resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.9.2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.2.0")

addSbtPlugin("de.oakgrove" % "sbt-hgid" % "0.3")

addSbtPlugin("de.oakgrove" % "sbt-brand" % "0.1")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")
