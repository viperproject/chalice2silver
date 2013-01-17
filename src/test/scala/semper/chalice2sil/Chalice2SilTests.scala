package semper.chalice2sil

import semper.sil.testing.DefaultSilSuite
import semper.sil.verifier.Verifier
import semper.source.Translator
import ch.ethz.inf.pm.silicon.SiliconVerifier

/**
  * @author Stefan Heule
  */
class Chalice2SilTests extends DefaultSilSuite {

  override def testDirectories: Seq[String] = Vector("benchmark", "translation")

  override def translator(verifier: Verifier, input: String): Translator = new Chalice2SilTranslator(verifier, input)

  override def verifiers: Seq[Verifier] = Vector(new SiliconVerifier())
}
