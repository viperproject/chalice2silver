Chalice2SIL - README
Christian Klauser <klauserc@student.ethz.ch>

Chalice2SIL is an alternative verification tool for Chalice.  Instead of 
targeting Boogie, it translates Chalice programs into SIL. By default, it uses
the symbolic verification tool "Silicon" to verify the input program.

================================================================================
1. Setup

Chalice2SIL comes with a set of shell scripts that make setup easier, but also 
impose some restrictions on how your directory structure is laid out.

--------------------------------------------------------------------------------
1.1 Quick Setup

  0.  Prerequisites. Make sure you have the following installed
        - Java 7 JDK, java.exe must be on the PATH
        - Z3, version 4.0
            Silicon is very particular about the location and version of Z3
        - Subversion. The `svn` tool must be on the PATH
        - Mercurial. The `hg` tool must be on the PATH
            Boogie lives in a mercurial repository
        - PowerShell (comes with Windows 7)
            By default, PowerShell does not execute unsigned shell scripts.
            See appendix A1.

  1.  Put this folder into a directory of it's own. The setup script will place 
      dependencies next to the Chalice2SIL folder.
      
      the-project-directory/
          Chalice2SIL/

  2.  Open a PowerShell and navigate to the-project-directory/Chalice2SIL

  3.  Have setup-repositories.ps1 download the source code for Chalice, SIL and 
      Silicon.

      PS> .\setup-repositories.ps1

      This will take a while. You should end up with a directory structure like 
      this:

      the-project-directory/
          Chalice2SIL/
          boogie/
              Chalice/
          silast
          silicon

  4.  Have build-dependencies.ps1 build Chalice, SIL and Silicon. 
      
      PS> .\build-dependencies.ps1

      This will take even longer, especially the first time around. This step
      has to be repeated every time one of Chalice2SIL's dependencies changes.

  5.  Build Chalice2SIL using sbt.ps1

  5a. Standalone *.jar file

      PS> .\sbt.ps1 assembly

      Creates the-project-directory/Chalice2SIL/target/chalice2sil.jar

      This archive is purely for convenience. It includes a (statically linked)
      copy of all libraries used by Chalice2SIL, except for Z3, of course.

  5b. Run unit-tests

      PS> .\sbt.ps1 test
      
      Will probably take a while and generate a lot of diagnostic output.

--------------------------------------------------------------------------------
1.2 Manual setup

    0.  Prerequisites. Make sure you have the following installed
        - Java 7 JDK
        - Z3, version 4.0
            Silicon is very particular about the location and version of Z3.

    1.  Get copies of the Chalice, SILAST and Silicon source code

    2.  Use SBT's publish-local task to build Chalice, SILAST and Silicon
        and have SBT put the results in your local ivy repository. 
        Make sure to publish SILAST *before* building Silicon.

    3.  Use SBT to build, test and/or run Chalice2SIL

    3a. PS> .\sbt.ps1 assembly
        Creates target/chalice2sil.jar which contains all prerequisite Java and 
        Scala libraries (even the Scala runtime itself).

    3b. PS> .\sbt.ps1 gen-idea
        Creates/updates project files for IntelliJ.

================================================================================
2. Using Chalice2SIL

    You can run Chalice2SIL via the assembled *.jar file using

    PS> java -Xmx512M -Xss512M -jar target/chalice2sil.jar <args...>

    or via SBT using

    PS> .\sbt.ps1 "run <args...>"

    Note that SBT expects the arguments to be together with the "run" task in 
    one command line argument. Also, SBT only works when you are currently in
    the Chalice2SIL directory.

    Alternatively, you can dot-source env.ps1 to get the Run-Chalice2SIL 
    function loaded into your shell:

    PS> . .\env.ps1
    (There are two dots in front of the \, separated by a space)

    After that, you can use

    PS> Run-Chalice2SIL -File "path\to\program.chalice" <args..>

    Run-Chalice2SIL internally uses SBT, but you do not need to be in the 
    Chalice2SIL directory. Don't forget to pass the chalice program via the
    -File parameter.

    Useful command line options:

      -v | --verbose
        Prints additional information about the translation/verification process

      -p | --print-sil
            Prints the translated program in SIL. SIL doesn't currently have a
            proper textual representation. The result might not be unambiguous.

      -f class name | --forward-sil class name
            Forwards the translated SIL program to the 
            `public static main(silAST.Program)` method of the specified class.
            When no -f switch is given, Chalice2SIL forwards the program to
            Silicon.

      -z3 <value> | --z3-path <value>
            Custom path to Z3, will be forwarded to Silicon

      -chop:<option>=<value> | --chalice-option:<option>=<value>
            Passes an option to Chalice. Can be specified multiple times. 
            A leading dash is added to the chalice option name automatically. 
            As only the Chalice parser and type-checker are used, options that
            only affect the Boogie-based verification will have no effect.    

      -? | --help
            Displays this help message.

================================================================================
A1. Have PowerShell execute unsigned scripts

    While PowerShell is installed on Windows 7, by default it refuses to execute
    script files without a cryptographic signature. In order to use the 
    PowerShell scripts provided with Chalice2SIL, you'll have to change* this 
    behaviour.

    1.  Open PowerShell as an administrator or with administrative privileges.
        You can do this by right-clicking on the PowerShell entry in the start 
        menu, for instance, and selecting the option with the shield icon next 
        to it.

    2.  PS> Set-ExecutionPolicy RemoteSigned

        Configures PowerShell to only require signatures for scripts that 
        come from a network location.

         -- OR --

         PS> Set-ExecutionPolicy Unrestricted

         Will still ask for confirmation on scripts that come from a network
         location

         -- OR --

         PS> Set-ExecutionPolicy Bypass

         Allow execution of all PowerShell scripts.

    Note: At ETH (and in other "corporate" environments) the desktop and user
    profile can be a "network location". In that case, the only real option may
    be "Bypass", unfortunately.

    * You could also sign the scripts using a certificate that you configured 
    PowerShell to trust. It is possible to create and install a certificate
    with just the tools available on Windows 7. Google will help you there.
