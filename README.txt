Chalice2SIL - README
Ioannis Kassios <ioannis.kassios@inf.ethz.ch>

The present version of Chalice2SIL is a translator from front-end verification
language Chalice to the intermediate representation language SIL, which is used
by static analysis tools, such as the symbolic execution engine Silicon.

Chalice2SIL is based on the MSc thesis of Christian Klauser.  Klauser's version
would also invoke Silicon on the produced program.  The present version is a
pure translator that does not depend on Silicon or any other SIL-related tool.

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

    java -jar %PROJECT_ROOT%/target/chalice2sil.jar <args...>

    or via SBT using

    sbt run <args...>

    Currently the only supported option is: --chop:<chalice-options> which
    passes options to the underlying Chalice parser.  The command also takes
    a single Chalice file.
	
    Should the parsing and resolution phase of Chalice be successful, the
    resulting SIL program will be pretty-printed and output at the standard
    output stream.  Otherwise, an error will be reported at the standard error
    stream.
	
  2.1 Running as a library:  To trigger a translation, use the following call:
	
    val (silProgram, messages) = new semper.chalice2sil.translation.
       ProgramTranslator(programOptions, chaliceFileName).translate(chaliceAST)
	  
    where:
      - silProgram: semper.sil.ast.Program
          is the resulting SIL program in AST form
      - messages: Seq[semper.chalice2sil.Message]
          is a sequence of warnings and errors produced during the translation
          process
      - programOptions: semper.chalice2sil.ProgramOptions
          contains a map of options for the translation
          (this feature is currently NOT USED -- use code ProgramOptions() to
          produce an empty options map)
      - chaliceFileName: String
          (optional) the file name of the Chalice program
      - chaliceAST: Seq[chalice.TopLevelDecl]
          is a Chalice AST produced by the Chalice parser and type checker
	

================================================================================
3. Testing

  3.0  The testing infrastructure and the unit test suite are currently under
    construction.  To test the whole test suite use command:
	
    sbt test
	
    The test suite consists of Chalice files and their corresponding
    pretty-printed SIL translations.  A test failure is reported if the output
    of Chalice2SIL does not match the corresponding translation.  If the
    Chalice program cannot be parsed or type-checked, or if there is no
    corresponding SIL program, the fact is reported, but not counted as a
    testing failure.