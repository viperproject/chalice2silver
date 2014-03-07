package semper.chalice2sil

import messages.ReportMessage
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import java.nio.file._
import collection.JavaConversions._
import java.util._
import java.io.File
import translation.ProgramTranslator
import chalice.Chalice

/*
import semper.sil.testing.DefaultSilSuite
import semper.sil.verifier.Verifier
import semper.silicon.Silicon
import semper.sil.frontend._
import java.io.File
import io.Source
import java.nio.file.Path

class Chalice2SILFrontEnd extends FrontEnd {
  val phases = List(
    Phase("parse and resolve", parseNResolve),
    Phase("translate", translate),
    Phase("verify", verify))

  var verf: Verifier = null

  var file: Path = null

  override def init(verifier: Verifier) { verf = verifier }

  override def reset(files: Seq[Path]) { file = files(0) }

  override def parseNResolve() ...

  override def translate() ...

  override def verify() ...
}

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
  ).map("src/test/resources/" + _)

  override def frontend(verifier: Verifier, files: Seq[Path]): Frontend = {
    val fe = new Chalice2SILFrontEnd()
    fe.init(verifier)
    fe.reset(files.head)
    fe
  }

  override def verifiers: Seq[Verifier] = Seq(new Silicon())
}
*/

/**
    Author: Yannis Kassios
 */
class C2STestSuite extends FunSuite with ShouldMatchers {
  val allTestDirectories = Seq(
    "basic",
    "oldC2SCases",
    "chaliceSuite/examples",
    "chaliceSuite/general-tests",
    "chaliceSuite/permission-model",
    "chaliceSuite/predicates",
    "chaliceSuite/regressions",
    "chaliceSuite/substantial-examples",
    "quantificationOverPermissions"
  ).map("src/test/resources/" + _)

  allTestDirectories foreach { name =>
    val ds = Files.newDirectoryStream(Paths.get(name), "*.chalice")
    ds foreach { p => testFile(p) }
  }

  def testFile(p: Path) {
    val chaliceFileName = p.toString
    var s: Scanner = null
    val fileName = chaliceFileName.substring(0, chaliceFileName.lastIndexOf('.'))

    val silFileContents = try {
      val f = fileName + ".sil"
      s = new Scanner(new File(f))
      s.useDelimiter("\\Z").next
    } catch {
       case e =>
         println("Cannot open expected SIL output for " + chaliceFileName)
         println("  " + e)
         return
    } finally { if(s!=null) s.close() }

    val expectedWarnings = try {
      val f = fileName + ".report"
      s = new Scanner(new File(f))
      s.useDelimiter("\\Z").next
    } catch {
       case e =>
        println("Cannot open expected warnings for " + chaliceFileName)
        println("  " + e)
        return
    } finally { if(s!=null) s.close() }

    val chaliceProgram = try {
      chalice.Chalice.parsePrograms(chalice.Chalice.parseCommandLine(Array("-noVerify", chaliceFileName)).get).get
    } catch {
       case e =>
         println("Cannot parse " + chaliceFileName)
         println("  " + e)
         return
    }

    if (!Chalice.typecheckProgram(null, chaliceProgram)) {
      Console.out.println(chaliceFileName + " contained type errors")
      return
    }

    val (silProgram, messages) = try {
      new ProgramTranslator(ProgramOptions(), chaliceFileName).translate(chaliceProgram)
    } catch {
       case e =>
         println("Translation for " + chaliceFileName + " failed")
         println("  " + e)
         return
    }

    test(chaliceFileName) { assert(silFileContents === silProgram.toString) }
    test(chaliceFileName + " warnings") {
      assert(expectedWarnings === messages.fold("", ((x: String, y: ReportMessage) => x + "\n" + y.toString)))
    }
  }
}
