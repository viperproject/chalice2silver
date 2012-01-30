param([Switch] $SkipCompile, [Switch] $SkipClean, [Switch] $SkipPackage)

$proj_root = "C:\Users\Christian\ETH\HS11\Bachelor";
$chalice_root = Join-Path $proj_root Boogie\Chalice;
$silast_root  = Join-Path $proj_root Chalice2SIL\silast\src\SILAST;

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

$sd = Get-ScriptDirectory;
Write-Output $sd;

$sbt = Join-Path $sd "chalice2sil\sbt.ps1";

Push-Location $chalice_root
    Write-Output "================== COMPILING CHALICE =============================="
    & $sbt "set scalaVersion := \`"2.9.1\`"" $cleanCmd $compileCmd $packageCmd #scala version needs to be escaped twice
    Write-Output "================== DONE COMPILING CHALICE ========================="
Pop-Location

Push-Location $silast_root
    Write-Output "================== COMPILING SILAST ==============================="
    & $sbt $cleanCmd $compileCmd $packageCmd
    Write-Output "================== DONE COMPILING SILAST =========================="
Pop-Location

$chalice_jar = Join-Path $chalice_root target\scala-2.9.1\chalice_2.9.1-1.0.jar
$silast_jar  = Join-Path $silast_root  target\scala-2.9.1\silast_2.9.1-0.1.jar

$lib_dir = "chalice2sil\lib";

if(-not (Test-Path $lib_dir)){
    New-Item -Type Container -Path $lib_dir | Out-Null;
}

Copy-Item $CHALICE_JAR (Join-Path $lib_dir "chalice.jar")
Copy-Item $SILAST_JAR  (Join-Path $lib_dir "silast.jar")

