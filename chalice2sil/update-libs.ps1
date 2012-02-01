param([Switch] $SkipCompile, [Switch] $SkipClean, [Switch] $SkipPackage, [String] $ScalaVersion = "2.9.1")

function Get-PWD # for some reason, this *must* be wrapped in a function, can't get it to work in the script itself.
{
    $Invocation = (Get-Variable MyInvocation -Scope 1).Value;
    (Split-Path $Invocation.MyCommand.Path)
}
$sd = Get-PWD;

$proj_root = Join-Path $sd "..";
$chalice_root = Join-Path $proj_root ..\Boogie\Chalice;
$silast_root  = Join-Path $proj_root silast\src\SILAST;
$chalice2sil_root = Join-Path $proj_root chalice2sil;

if($SkipCompile){
    $compileCmd = "";
}else{
    $compileCmd = "compile";
}

if(($SkipCompile) -or ($SkipClean)){
    $cleanCmd = "";
}else{
    $cleanCmd = "clean";
}

if($SkipPackage){
    $packageCmd = "";
} else {
    $packageCmd = "package";
}

$sbt = Join-Path $chalice2sil_root "sbt.ps1";

Push-Location $chalice_root
    Write-Output "================== COMPILING CHALICE =============================="
    & $sbt "set scalaVersion := \`"$ScalaVersion\`"" $cleanCmd $compileCmd $packageCmd #scala version needs to be escaped twice
    Write-Output "================== DONE COMPILING CHALICE ========================="
Pop-Location

Push-Location $silast_root
    Write-Output "================== COMPILING SILAST ==============================="
    & $sbt $cleanCmd $compileCmd $packageCmd
    Write-Output "================== DONE COMPILING SILAST =========================="
Pop-Location

$chalice_jar = Join-Path $chalice_root "target\scala-$ScalaVersion\chalice_$ScalaVersion-1.0.jar"
$silast_jar  = Join-Path $silast_root  "target\scala-$ScalaVersion\silast_$ScalaVersion-0.1.jar"

$lib_dir = Join-Path $chalice2sil_root lib

if(-not (Test-Path $lib_dir)){
    New-Item -Type Container -Path $lib_dir | Out-Null;
}

Copy-Item $CHALICE_JAR (Join-Path $lib_dir "chalice.jar")
Copy-Item $SILAST_JAR  (Join-Path $lib_dir "silast.jar")

