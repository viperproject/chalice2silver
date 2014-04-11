/**
 * Author: Yannis Kassios (based on an older version by Christian Klauser)
 */

package semper.chalice2sil

import scopt._
import chalice.Chalice
import translation.ProgramTranslator
import java.io.{FileWriter, PrintWriter, File}
import semper.sil.verifier.{Failure, Success, Verifier}

// **
// Options to pass to the Chalice library; together with an optional filename
// This translation parameter is currently not used, except to pass the -noVerify flag.
// It may be removed in the future, unless a need to pass options to Chalice shows up.
// **
case class ProgramOptions(chaliceOptions: Map[String, String] = Map[String, String](),
                          chaliceFile: File = null,
                          silFile: File = null,
                          showVersion: Boolean = false,
                          backendClass: Option[String] = None)

// **
// Translate one file from command line
// **
object Program {
  def main(args: Array[String]) {
    // Option parser
    val cmdParser = new OptionParser[ProgramOptions]("chalice2sil") {
      // Chalice file
      arg[File]("<chalice-file>") optional() action {
        (source, c) => c.copy(chaliceFile = source)
      } text ("The chalice source file.")

      arg[File]("<sil-file>")
        .optional()
        .action{(dest, c) => c.copy(silFile = dest)}
        .text("The SIL output file. If omitted, results will be sent to stdio.")

      opt[Unit]("version")
        .action{(_, c) => c.copy(showVersion = true)}
        .text("Display version information")

      opt[String]("backend")
        .action{(className, c) => c.copy(backendClass = Some(className))}
        .text(  "Forward SIL AST to specified backend. The full-qualified name of a class "
              + s"extending ${classOf[semper.sil.verifier.Verifier].getName} is expected. "
              + "The class must be on the classpath.")

      checkConfig{c =>
        if (!c.showVersion && c.chaliceFile == null)
          failure("Missing argument <chalice-file>")
        else success
      }

      // Help
      help("?") text ("Displays this help message.")
    }
    var progOptions = cmdParser.parse(args, ProgramOptions()) map { opts => opts } getOrElse { return }

    if (progOptions.showVersion) {
      import brandingData._
      println(s"$sbtProjectName $sbtProjectVersion (${hgid.version} $buildDate)")
      return
    }

    val chalice2SIL = new Chalice2SILFrontEnd()

    val verifierOrNull = progOptions.backendClass match {
      case Some(className) =>
        val verifier = Class.forName(className).newInstance.asInstanceOf[Verifier]
        verifier.parseCommandLine(Nil)

        verifier

      case None => null
    }

    chalice2SIL.init(verifierOrNull)
    chalice2SIL.reset(Seq(progOptions.chaliceFile.toPath))
    chalice2SIL.run()

    if (chalice2SIL.failed.nonEmpty) {
      chalice2SIL.failed.foreach(f => Console.err.println("[Chalice2SIL] " + f))
      Console.err.println("Chalice2SIL produced %s.".format(pluralize("error", chalice2SIL.failed.length)))
      return
    }

    /* Report translation messages */

    val messages = chalice2SIL.messages

    if (messages.nonEmpty) {
      messages.foreach(m => Console.err.println("[Chalice2SIL] " + m))
      Console.err.println("Chalice2SIL produced %s.".format(pluralize("message", messages.length)))
    }

    /* Print generated SIL program */

    progOptions.silFile match {
      case null =>
        if (progOptions.backendClass.isEmpty) println(chalice2SIL.silAST)

      case file =>
        val sink = new PrintWriter(new FileWriter(file))
        sink.println(chalice2SIL.silAST)
        sink.close()

        println(s"Wrote generated SIL program to ${progOptions.silFile}")
    }

    /* Report verification result */

    if (verifierOrNull != null) chalice2SIL.verifierResult match {
      case Success => println("Verification succeeded")

      case Failure(errors) =>
        errors foreach (e => println(e.readableMessage))
        println(s"Verification failed with ${pluralize("error", errors.length)}")
    }
  }

  def pluralize(noun: String, count: Int) = count match {
    case 0 => "no " + noun + "s"
    case 1 => "1 " + noun
    case n => n.toString + " " + noun + "s"
  }
}
