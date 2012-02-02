param([Switch] $WhatIf)

#
#Run-Chalicerequires the following (global) variables to be set
#  $CHALICE2SIL_PROJECT
#       The path to the folder that contains the Chalice2SIL source code
#
#  $CHALICE_JAR
#       The path to the chalice jar
#
#
#One way to define global variables is the following syntax
#    PS> New-Variable -Scope Global SCALA_LIB "path\to\scala\lib"
#



function Get-PWD # for some reason, this *must* be wrapped in a function, can't get it to work in the script itself.
{
    $Invocation = (Get-Variable MyInvocation -Scope 1).Value;
    (Split-Path $Invocation.MyCommand.Path)
}
$sd = Get-PWD;

$sbt = Join-Path $sd "sbt.ps1";
$sargs = $args | foreach { $s = $_.Replace("\","\\").Replace("`"","\`""); "$s" }
$eargs = [String]::Join(" ", $sargs)

if($WhatIf){
    echo $eargs
    echo "$sbt `"run $eargs`""
} else {
    & $sbt "run $eargs"
}