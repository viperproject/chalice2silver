package ch.ethz.inf.pm.semper.chalice2sil

import semper.sil.ast.programs.{Program => SilProgram}
import ch.ethz.inf.pm.semper.chalice2sil.{Program => Chalice2Sil}
import scala.collection.JavaConversions._
import org.scalatest._
import translation.ProgramEnvironment
import ch.ethz.inf.pm.silicon.{Silicon, Config}
import io.Source
import java.nio.file.{Path, Files, Paths}
import ch.ethz.inf.pm.silicon.interfaces.ResultWithMessage
import semper.sil.ast.source.SourceLocation
import runtime.ScalaRunTime
import java.util.Locale

/**
  * Author: Christian Klauser
  */

/**
  * A functional test suite backed by a directory with *.chalice files. It will turn all *.chalice files in that
  * directory into a test. You can optionally attach additional assertions to each of these files.
  * The \@RunWith annotation is only necessary when you want to run this suite in JUnit-based runners.
  * {{{
  *   package ch.ethz.inf.pm.semper.chalice2sil
  *   import org.scalatest.junit.JUnitRunner
  *
  *   @RunWith(classOf[JUnitRunner])
  *   class Translation extends ChaliceSuite {
  *
  *     'local.chalice {
  *       assert(program.methods != null) //`program` contains the translated AST
  *       assert(messages.isEmpty)        //`messages` contains the generated messages
  *     }
  *
  *   }
  * }}}

  }*/
abstract class ChaliceSuite extends FunSuite { //
  /**
    * The directory this test suite is associated with. Defaults to the fully qualified name of your test suite with dots replaced by slashes.
    * You are welcome to override this in subclasses.
    */
  val directoryPath = Paths.get(getClass.getName.replace(".","/") + "_")

  /**
    * The same as `directoryPath`, just interpreted as a path relative to the test resources root and converted to
    * an absolute path. Always use `absoluteDirectoryPath` when trying to open files etc. `directoryPath` is just
    * for initialization and human readability.
    * @see directoryPath
    */
  protected lazy val absoluteDirectoryPath = { //
    val rootUrl = getClass.getClassLoader.getResource("index.toc")
    Predef.assert(rootUrl != null,"Cannot find unit test resource root.")
    val root = Paths.get(rootUrl.toURI).getParent
    root.resolve(directoryPath)
  }

  /**
    * The SIL AST for the translated Chalice program.
    */
  var program : SilProgram = null

  /**
    * The translation environment for the program. (Provides access to the prelude, among other things)
    */
  var environment : ProgramEnvironment = null

  /**
    * The sequence of messages generated by Chalice2Sil.
    */
  var messages : Seq[Message] = null

  /**
    * The list of results returned by Silicon
    */
  var results : List[ch.ethz.inf.pm.silicon.interfaces.VerificationResult] = null

  /**
    * Returns the prelude for the current program
    * @return The prelude for the current program.
    * @throws IllegalArgumentException environment is not set (i.e. not called from within a test)
    * @see program
    */
  def prelude = {
    require(environment != null)
    environment.prelude
  }
  
  private val explicitFiles = collection.mutable.Set[Symbol]()
  
  protected class ChaliceSymbolWrapper(symbol : Symbol) {
    def chalice(t : => Unit) { //
      explicitFiles += symbol
      val fileName = symbol.name + ".chalice"

      /**
       * The error messages expected by the current test.
       */
      val expectedResults : collection.mutable.Set[ExpectedSiliconMessage] = collection.mutable.Set()

      /**
       * Flag that indicates that verification results are not relevant for this test.
       */
      var ignoreVerificationResults : Boolean = false

      def expectedResultsFromSource(path : Path) {
        var lineNum = 1
        for(line <- Source.fromFile(path.toUri,scala.io.Codec.UTF8.name).getLines()){
          val mat = expectedErrorFormat.pattern.matcher(line)
          while(mat.find()){
            val code = Integer.parseInt(mat.group(1))
            expectedResults += ExpectedSiliconMessage(lineNum, code)
          }
          if(ignoreFormat.findFirstIn(line).isDefined){
            ignoreVerificationResults = true
          }
          lineNum += 1
        }
      }

      val filePath = absoluteDirectoryPath.resolve(fileName)

      val opts = new ProgramOptions
      opts.chaliceFiles += filePath.toString
      opts.printSil = true

      expectedResultsFromSource(filePath)

      def buildFunc(name : String)(body : => Unit) {
        if(ignoreVerificationResults) ignore(name)(body) else test(name)(body)
      }

      buildFunc(fileName){
        val beforeChalice = System.currentTimeMillis()
        Chalice2Sil.invokeChalice(opts) match {
          case None =>
            fail("Chalice failed to parse/typecheck file %s. See Stdout for more details.".format(fileName))
          case Some(p) =>
            val afterChalice = System.currentTimeMillis()
            val translator = Chalice2Sil.createTranslator(opts,p)
            val (silProgram, silMessages) = translator.translate(p)
            val afterSIL = System.currentTimeMillis()

            //Console.out.println(silProgram.toString())

            silMessages.view.filter(_.severity.indicatesFailure).foreach(m => fail("Detected message that indicates failure: %s".format(m)))

            //now apply the additional assertions
            Predef.assert(program == null,"program is still assigned.")
            Predef.assert(messages == null, "messages is still assigned.")
            Predef.assert(environment == null, "environment is still assigned.")
            Predef.assert(results == null, "results is still assigned.")

            //Console.out.println("Passing SIL program to Silicon")
            var beforeSilicon : Long = 0
            var afterSilicon : Long = 0
            val siliconResults = {
              val config = new Config(z3exe = DefaultConfig.z3path.toAbsolutePath.toString, logLevel="ERROR")

              beforeSilicon = System.currentTimeMillis()
              val silicon = new Silicon(config)

              val r = silicon.execute(silProgram)
              afterSilicon = System.currentTimeMillis()
              r
            }
            // results have already been printed
            
            program = silProgram
            messages = silMessages  
            environment = translator
            results = siliconResults

            try{              
              t
              //TODO: filter out expected errors
              val unexpected = results.map({
                case wm:ch.ethz.inf.pm.silicon.interfaces.ResultWithMessage =>
                  expectedResults
                    .find(e => e.code == wm.message.code && lineFromLoc(wm.message.loc) == e.line) match {
                    case Some(m) =>
                      expectedResults -= m
                      None
                    case None =>
                      Some(wm)
                  }
                case o => Some(o)
              }).flatten
              
              unexpected foreach { result =>
                fail("Detected fatal result from Silicon: %s".format(result))
              }
              expectedResults foreach { e =>
                fail("Expected result %s but no such message was emitted.".format(e))
              }
            } finally {
              program = null
              messages = null
              environment = null
              results = null

              // print timings
              //Console.err.println("Chalice: %d  ms".format(afterChalice-beforeChalice))
              //Console.err.println("Chalice2SIL: %d  ms".format(afterSIL - afterChalice))
              //Console.err.println("Silicon: %d  ms".format(afterSilicon- beforeSilicon))
            }
        }
      }
    }

    private val locationFormat = """(\d+)\.(\d+)""".r
    private def lineFromLoc(loc : SourceLocation) : Int = locationFormat findFirstIn loc.toString match {
      case None => -1
      case Some(locationFormat(line,col)) => Integer.parseInt(line)
    }

    private val expectedErrorFormat = "@Error (\\d+)".r
    private val ignoreFormat = "@Ignore".r
  }

  protected implicit def symbolToChaliceSymbolWrapper(symbol : Symbol) : ChaliceSymbolWrapper = new ChaliceSymbolWrapper(symbol)

  /**
    * Invoked during initialization to generate tests for all *.chalice files in the directory
    * that were not mentioned in the test suite. If you don't want this behaviour, just override this
    * method with an empty body.
    */
  protected def translateImplicitFiles() { //
    require(!implicitFilesRegistered,"implicit files already registered.")

    val dirStream = Files.newDirectoryStream(absoluteDirectoryPath,"*.chalice")
    val implicitFiles = try { //
      dirStream
        .map(p => Symbol(p.getFileName.toString.dropRight(".chalice".length)))
        .filterNot(explicitFiles.contains(_))
        .toList
    } finally { //
      dirStream.close()    
    }

    val doNothing = {}
    implicitFiles.foreach(_.chalice(doNothing))

    implicitFilesRegistered = true
  }
  
  private var implicitFilesRegistered = false
  protected final def ensureImplicitFilesRegistered(){
    if(implicitFilesRegistered) return
    
    translateImplicitFiles()
  }

  override def testNames = {
    ensureImplicitFilesRegistered()
    super.testNames
  }

  protected override def runTest(testName : String, reporter : Reporter, stopper : Stopper, configMap : Map[String, Any], tracker : Tracker) {//
    ensureImplicitFilesRegistered()
    super.runTest(testName, reporter, stopper, configMap, tracker)
  }

  protected override def runTests(testName : Option[String], reporter : Reporter, stopper : Stopper, filter : Filter, configMap : Map[String, Any], distributor : Option[Distributor], tracker : Tracker) { //
    ensureImplicitFilesRegistered()
    super.runTests(testName, reporter, stopper, filter, configMap,distributor, tracker)
  }

  override def run(testName : Option[String], reporter : Reporter, stopper : Stopper, filter : Filter, configMap : Map[String, Any], distributor : Option[Distributor], tracker : Tracker) {//
    ensureImplicitFilesRegistered()
    super.run(testName,reporter, stopper, filter, configMap, distributor,  tracker)
  }
}
