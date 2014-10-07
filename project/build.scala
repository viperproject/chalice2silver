import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import de.oakgrove.SbtBrand.{BrandKeys, brandSettings, Val, BrandObject}
import de.oakgrove.SbtHgId.{HgIdKeys, hgIdSettings}

object Chalice2SilBuild extends Build {

  /* Base settings */

  lazy val baseSettings = (
       Defaults.defaultSettings
    ++ hgIdSettings
    ++ brandSettings
    ++ Seq(
          organization := "viper",
          version := "0.1-SNAPSHOT",
          // publishArtifact in packageDoc := false,
          scalaVersion := "2.10.4",
          // publishMavenStyle := false,
          // componentID := None,
          // crossPaths := false,
          // testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-w", "1"),
          // javacOptions in Compile ++= Seq("-target", "6", "-source", "6")
          scalacOptions in Compile ++= Seq("-deprecation", "-unchecked", "-feature"),
          resolvers += "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"))

/* Projects */

  lazy val chalice2sil = {
    var p = Project(
      id = "chalice2sil",
      base = file("."),
      settings = (
           baseSettings
        ++ assemblySettings
        ++ Seq(
              name := "Chalice2Sil",
              mainClass in (Compile, run) := Some("viper.chalice2sil.Program"),
              mainClass in assembly := Some("viper.chalice2sil.Program"),
              jarName in assembly := "chalice2sil.jar",
              test in assembly := {},
              testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
              traceLevel := 10,
              maxErrors := 6,
              javaOptions in run ++= Seq("-Xss128M", "-Dfile.encoding=UTF-8"),
              javaOptions in Test += "-Xss128M",
              fork := true,
              //classDirectory in Test <<= classDirectory in Compile,
              libraryDependencies ++= externalDep,
              BrandKeys.dataPackage := "viper.chalice2sil",
              BrandKeys.dataObject := "brandingData",
              BrandKeys.data += Val("buildDate", new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new java.util.Date)),
              BrandKeys.data <+= scalaVersion(Val("scalaVersion", _)),
              BrandKeys.data <+= sbtBinaryVersion(Val("sbtBinaryVersion", _)),
              BrandKeys.data <+= sbtVersion(Val("sbtVersion", _)),
              BrandKeys.data <+= name(Val("sbtProjectName", _)),
              BrandKeys.data <+= version(Val("sbtProjectVersion", _)),
              BrandKeys.data <+= HgIdKeys.projectId { idOrException =>
                val hgid =
                  idOrException.fold(Predef.identity,
                                     _ => de.oakgrove.SbtHgId.Id("<unknown", "<unknown", "<unknown", "<unknown"))
                BrandObject("hgid",
                            """val version = "%s"
                               val id = "%s"
                               val branch = "%s"
                               val tags = "%s"
                            """.format(hgid.version, hgid.id, hgid.branch, hgid.tags))
              },
              sourceGenerators in Compile <+= BrandKeys.generateDataFile))
    ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)
    for (dep <- internalDep) {
      p = p.dependsOn(dep)
    }
    p
  }

  // On the build-server, we cannot have all project in the same directory, and thus we use the publish-local mechanism for dependencies.
  def isBuildServer = sys.env.contains("BUILD_TAG") // should only be defined on the build server
  def internalDep = if (isBuildServer) Nil else Seq(libs.chaliceDir, libs.silDir, libs.siliconDir)
  def externalDep = {
    Seq(libs.scopt) ++
    (if (isBuildServer) Seq(libs.chalice, libs.silver, libs.silicon) else Nil)
  }



  object libs {
    lazy val silver = "viper" %% "silver" %  "0.1-SNAPSHOT"
    lazy val chalice = "ychalice" %% "ychalice" % "1.0"
    lazy val silicon = "viper" %% "silicon-quantified-permissions" %  "0.1-SNAPSHOT"

    lazy val silDir = RootProject(new java.io.File("../Silver"))
    lazy val chaliceDir = RootProject(new java.io.File("../Chalice"))
    lazy val siliconDir = RootProject(new java.io.File("../Silicon"))

    lazy val scopt = "com.github.scopt" %% "scopt" % "3.2.0"
  }
}
