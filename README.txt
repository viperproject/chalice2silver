Chalice2SIL - README
Ioannis Kassios <ioannis.kassios@inf.ethz.ch>

Chalice2SIL is a translator from front-end verification language Chalice to the
intermediate representation language SIL, which is used by static analysis
tools, such as the symbolic execution engine Silicon.  Chalice2SIL is based on
the MSc thesis of Christian Klauser.

In comparison with the previous version, Chalice2SIL now supports the new SIL
AST library, as well as extensions to the Chalice language that are not
supported by the main branch of Chalice in Codeplex.  Note that some old
Chalice features are deprecated in the new version.

Chalice2SIL is currently under construction.  Many features are not yet
supported and/or adequately tested.


================================================================================
0. Setup

  0.0  Prerequisites: Make sure you have the following installed
        - Java 7 JDK
        - Scala 2.10
        - SBT
        
  0.1.  Clone the repository under a parent folder, from now on referred to as
        %PROJECT_ROOT%
      
        %PROJECT_ROOT%/
          chalice2sil/
		
  0.2   Clone the SIL AST library repository in its own folder named sil under
        %PROJECT_ROOT%
  
  0.3   Clone the Chalice repository in its own folder named chalice under
        %PROJECT_ROOT%
		
        ***IMPORTANT*** Make sure you clone the correct Chalice branch.  The
        main Chalice branch maintained by Microsoft does not have many features
        required by Chalice2SIL.  The correct branch can be found under:

        https://chalice.codeplex.com/SourceControl/network/forks/ykass/PuQBP
		

================================================================================
1. Building 
		
  1.0 Use the command:
  
      sbt compile
	  
      to produce the following executable jar:
  
      %PROJECT_ROOT%/target/chalice2sil.jar

      If you want a jar file that contains all the dependencies of the project,
      then use the command:
	  
      sbt assembly


================================================================================
2. Using

  2.0  Running as an independent program: You can run Chalice2SIL via the
    assembled *.jar file using

    java -jar %PROJECT_ROOT%/target/chalice2sil.jar [options] [<chalice-file>] [<sil-file>]

    or via SBT using

    sbt run [options] [<chalice-file>] [<sil-file>]
	
	The command line arguments are explained below:
	
	<chalice-file>
  	    The chalice source file.
    <sil-file>
        The SIL output file. If omitted, results will be sent to stdio.
    --version
        Display version information.
    --backend <value>
        Forward SIL AST to specified backend.  The full-qualified name of a
		class extending semper.sil.verifier.Verifier is expected. The class
		must be on the classpath.
    --xml <value>
        Write the verification results to the specified XML file. The
		structure of the generated XML document is similar to that of Boogie
		when run with -xml.Requires --backend, there otherwise won't be any
		verification results.
    --?
        Displays help.

    Errors and warnings are reported at the standard error stream.
	
  2.1 Running as a library:  To trigger a translation, use the following call:
	
    val (silProgram, messages) = new semper.chalice2sil.translation.
       new ProgramTranslator(chaliceFilename).translate(chaliceAST)
	  
    where:
      - silProgram: semper.sil.ast.Program
          is the resulting SIL program in AST form
      - messages: Seq[semper.chalice2sil.ReportMessage]
          is a sequence of warnings and errors produced during the translation
          process
      - chaliceFilename: String
          (optional) the file name of the Chalice program
      - chaliceAST: Seq[chalice.TopLevelDecl]
          is a Chalice AST produced by the Chalice parser and type checker
	

================================================================================
3. Testing

  3.0  The testing infrastructure and the unit test suite are currently under
    construction.