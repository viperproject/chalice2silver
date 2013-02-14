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

  override def testDirectories: Seq[String] =
    Vector("benchmark",
           "translation",
           "syxc/fast/basics",
           "syxc/fast/branching",
           "syxc/fast/bugs",
           "syxc/fast/channels")

  override def translator(verifier: Verifier, input: String): Translator =
    new Chalice2SilTranslator(verifier, input)

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

    /* Filter all configMap entries where the key starts with 'silicon:'. Such entries are
     * forwarded to Silicon as (command-line) arguments after the prefix 'silicon:' has been
     * removed.
     */
    val prefix = "silicon:"

    val args: Seq[String] =
      configMap.filter{case (k, _) => k.startsWith(prefix)}
               .flatMap{case (k, v) => Seq("--" + k.replace(prefix, ""), v.toString)}
               .toSeq

    new Silicon(args)
  }
}
