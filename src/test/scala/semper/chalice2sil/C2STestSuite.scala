package semper.chalice2sil

/**
Author: Yannis Kassios
*/

import java.nio.file.Path
import semper.sil.testing._
import semper.sil.verifier._
import semper.silicon.Silicon
import semper.sil.frontend._

class AllTests extends DefaultSilSuite {
  override lazy val testDirectories: Seq[String] = Seq(
    "basic",
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model",
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples",
    "quantificationOverPermissions"
  )

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files)
    fe
  }

  override val verifiers = Seq(
    new SiliconVerifierFactory(optionsFromScalaTestConfigMap(configMap))
  )

  override val defaultTestPattern: String = ".*\\.chalice"

  private def optionsFromScalaTestConfigMap(configMap: Predef.Map[String, Any]): Seq[String] = {
    val prefix = "silicon:"

    configMap.flatMap {
      case (k, v) if k.startsWith(prefix) => Seq("--" + k.substring(prefix.length), v.toString)
      case _ => Seq()
    }.toSeq
  }

  // this class serves as an adaptor for Silicon, to bypass the fact that a Silicon instance may only run once
  class SiliconVerifierFactory(args:Seq[String]) extends Silicon {
    override def verify(program: semper.sil.ast.Program) = {
      val silicon = new Silicon()
      silicon.parseCommandLine(args)
      silicon.config
      silicon.verify(program)
    }
  }
}
