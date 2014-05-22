package semper.chalice2sil

/**
Author: Yannis Kassios
*/

import java.nio.file.Path
import semper.sil.testing._
import semper.sil.verifier._
import semper.silicon.Silicon
import semper.sil.frontend._

class AllTests extends SilSuite {
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

  override val verifiers = Seq(SiliconVerifierFactory)

  override val defaultTestPattern: String = ".*\\.chalice"

  // this class serves as an adaptor for Silicon, to bypass the fact that a Silicon instance may only run once
  object SiliconVerifierFactory extends Silicon {
    override def verify(program: semper.sil.ast.Program) = {
      val silicon = new Silicon()
      silicon.parseCommandLine(Seq())
      silicon.config
      silicon.verify(program)
    }
  }
}
