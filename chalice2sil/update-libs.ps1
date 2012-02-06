param([Switch] $SkipCompile, [Switch] $SkipClean, [Switch] $SkipPackage, [String] $ScalaVersion = "2.9.1")

function Get-PWD # for some reason, this *must* be wrapped in a function, can't get it to work in the script itself.
{
    $Invocation = (Get-Variable MyInvocation -Scope 1).Value;
    (Split-Path $Invocation.MyCommand.Path)
}
$sd = Get-PWD;

$proj_root = Join-Path $sd "..";
$chalice_root = Join-Path $proj_root ..\Boogie\Chalice;
$chalice_version = "1.0";
$silast_root  = Join-Path $proj_root silast\src\SILAST;
$silast_version = "0.1";
$chalice2sil_root = Join-Path $proj_root chalice2sil;
$chalice2sil_version = "0.1-SNAPSHOT";
$silicon_root = Join-Path $proj_root Silicon;
$silicon_version = "0.1-SNAPSHOT";

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

$chalice_jar = Join-Path $chalice_root "target\scala-$ScalaVersion\chalice_$ScalaVersion-$chalice_version.jar"
$silast_jar  = Join-Path $silast_root  "target\scala-$ScalaVersion\silast_$ScalaVersion-$silast_version.jar"
$silicon_jar = Join-Path $silicon_root "target\scala-$ScalaVersion\silicon_$ScalaVersion-$silicon_version.jar"

function Provide-Lib($root,$jar) {
    $lib_dir = Join-Path $root lib;
    if(-not (Test-Path $lib_dir)){
        New-Item -Type Container -Path $lib_dir | Out-Null;
    }
    $lib = Get-Item $jar;
    $idx = $lib.BaseName.IndexOf("_");
    $lib_name = $lib.BaseName.Substring(0,$idx);
    Copy-Item $lib (Join-Path $lib_dir "$lib_name.jar");
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

Provide-Lib  $silicon_root $silast_jar;

Push-Location $silicon_root
    Write-Output "================== COMPILING SILICON =============================="
    & $sbt $cleanCmd $compileCmd $packageCmd
    Write-Output "================== DONE COMPILING SILICON ========================="
Pop-Location

Provide-Lib $chalice2sil_root $chalice_jar;
Provide-Lib $chalice2sil_root $silast_jar;
Provide-Lib $chalice2sil_root $silicon_jar;
