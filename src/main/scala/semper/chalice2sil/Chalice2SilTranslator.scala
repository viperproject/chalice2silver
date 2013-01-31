package semper.chalice2sil

import chalice.TopLevelDecl
import semper.sil.ast.programs.{Program => SILProgram}
import semper.source.DefaultTranslator
import semper.sil.verifier.{VerificationResult, Verifier}
import java.io.{PrintStream, FileOutputStream, File}

/** A translator from Chalice to SIL.
 *
 * @author Stefan Heule
 */
class Chalice2SilTranslator(verifier: Verifier, input: String) extends DefaultTranslator(verifier, input) {

  var chaliceProgram: List[TopLevelDecl] = null
  var silProgram: SILProgram = null
  val progOpts = new ProgramOptions()

  def doParse() {
    // TODO: it would be better if we can pass the input to Chalice instead of writing to a temporary file
    val tempFile = File.createTempFile("tmp",".chalice")
    val fout = new FileOutputStream(tempFile)
    val out = new PrintStream(fout)
    out.print(input)
    out.close()

    // TODO: separate Chalice parsing and type-checking
    progOpts.z3path = Some(DefaultConfig.z3path.toAbsolutePath.toString)
    progOpts.chaliceFiles.append(tempFile.getAbsolutePath)
    semper.chalice2sil.Program.invokeChalice(progOpts) match {
      case None =>
      case Some(p) => chaliceProgram = p
    }

    tempFile.delete()
  }

  def doTypecheck() {}

  def doTranslate(): SILProgram = {
    val (p, messages) = semper.chalice2sil.Program.translateToSil(progOpts, chaliceProgram)
    silProgram = p
    p
  }

  protected def mapVerificationResult(in: VerificationResult): VerificationResult = in
}
