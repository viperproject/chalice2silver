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
          organization := "ch.ethz.inf.pm",
          version := "0.1-SNAPSHOT",
          // publishArtifact in packageDoc := false,
          scalaVersion := "2.10.3",
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
              mainClass in (Compile, run) := Some("semper.chalice2sil.Program"),
              mainClass in assembly := Some("semper.chalice2sil.Program"),
              jarName in assembly := "chalice2sil.jar",
              test in assembly := {},
              testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),
              traceLevel := 10,
              maxErrors := 6,
              classDirectory in Test <<= classDirectory in Compile,
              libraryDependencies ++= externalDep,
              BrandKeys.dataPackage := "semper.chalice2sil",
              BrandKeys.dataObject := "brandingData",
              BrandKeys.data += Val("buildDate", new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm:ss").format(new java.util.Date)),
              BrandKeys.data <+= scalaVersion(Val("scalaVersion", _)),
              BrandKeys.data <+= sbtBinaryVersion(Val("sbtBinaryVersion", _)),
              BrandKeys.data <+= sbtVersion(Val("sbtVersion", _)),
              BrandKeys.data <+= name(Val("sbtProjectName", _)),
              BrandKeys.data <+= version(Val("sbtProjectVersion", _)),
              BrandKeys.data <+= HgIdKeys.projectId { hgid =>
                BrandObject("hgid",
                            """val version = "%s"
                               val id = "%s"
                               val branch = "%s"
                               val tags = "%s"
                            """.format(hgid.version, hgid.id, hgid.branch, hgid.tags))
              },
              sourceGenerators in Compile <+= BrandKeys.generateDataFile))
    )
    for (dep <- internalDep) {
      p = p.dependsOn(dep)
    }
    p
  }

  // On the build-server, we cannot have all project in the same directory, and thus we use the publish-local mechanism for dependencies.
  def isBuildServer = sys.env.contains("BUILD_TAG") // should only be defined on the build server
  def internalDep = if (isBuildServer) Nil else Seq(libs.chaliceDir, libs.silDir, libs.siliconDir)
  def externalDep = {
    Seq(libs.scopt, libs.scalatest, libs.junit) ++
    (if (isBuildServer) Seq(libs.chalice, libs.sil, libs.silicon) else Nil)
  }

  object libs {
    lazy val sil = "semper" %% "sil" %  "0.1-SNAPSHOT"
    lazy val chalice = "chalice" %% "chalice" %  "1.0"
    lazy val silicon = "semper" %% "silicon" %  "0.1-SNAPSHOT"
    
    lazy val silDir = RootProject(new java.io.File("../Sil"))
    lazy val chaliceDir = RootProject(new java.io.File("../Chalice"))
    lazy val siliconDir = RootProject(new java.io.File("../Silicon"))

    lazy val scalatest = "org.scalatest" %% "scalatest" % "1.8" % "test" withJavadoc() withSources()
    lazy val scopt = "com.github.scopt" %% "scopt" % "3.2.0"
    lazy val junit = "junit" % "junit" % "4.8.1" % "test"
      /* JUnit seems to only be required by semper.chalice2sil.util.UnicodeManglerTests. */
  }
}