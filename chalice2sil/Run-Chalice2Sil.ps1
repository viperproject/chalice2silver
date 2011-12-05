#
#Run-Chalicerequires the following (global) variables to be set
#  $CHALICE2SIL_PROJECT
#       The path to the folder that contains the Chalice2SIL source code
#
#  $SCALA_LIB
#       The path to the folder that contains the scala-library.jar
#
#  $CHALICE_JAR
#       The path to the chalice jar
#
#
#One way to define global variables is the following syntax
#    PS> New-Variable -Scope Global SCALA_LIB "path\to\scala\lib"
#

$proj  = $CHALICE2SIL_PROJECT;
$classpath = (
    (Join-Path $proj chalice2sil\bin),
    (Join-Path $proj chalice2sil\lib\scopt_2.9.1-1.1.2.jar),
    (Join-Path $SCALA_LIB scala-library.jar)
    # Reference to Chalice missing
);
$classpath = Join-String $classpath -Separator ";";
$package = "ch.ethz.inf.pm.semper.chalice2sil";
java -classpath "$classpath" -ea:$package "$package.Program" $args