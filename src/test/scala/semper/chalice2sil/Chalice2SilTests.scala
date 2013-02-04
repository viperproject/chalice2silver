package semper.chalice2sil

import semper.sil.testing.DefaultSilSuite
import semper.sil.verifier.Verifier
import semper.source.Translator
import semper.silicon.Silicon

/** All tests for chalice2sil.
  *
  * @author Stefan Heule
  */
class Chalice2SilTests extends DefaultSilSuite {

  override def testDirectories: Seq[String] = Vector("benchmark", "translation")

  override def translator(verifier: Verifier, input: String): Translator = new Chalice2SilTranslator(verifier, input)

  override def verifiers: Seq[Verifier] = Vector(instantiateSilicon())

  private def instantiateSilicon() = {
    /* ScalaCheck supports passing arguments to test suites via -Dkey=value. However,
     * since keys may not start with '--' and since Silicon's options all start with '--',
     * we have to prepend the prefix here.
     */
    assert(
      configMap != null,
      "configMap is null, but should have been initialised in underlying ScalaTest class (" +
      "semper.sil.testing.SilSuite?)")

    val args: Seq[String] = configMap.flatMap{case (k, v) => Seq("--" + k, v.toString)}.toSeq

    new Silicon(args)
  }
}
