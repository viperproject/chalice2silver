param([Switch] $SkipCompile)

$proj_root = "C:\Users\Christian\ETH\HS11\Bachelor";
$chalice_root = Join-Path $proj_root Boogie\Chalice;
$silast_root  = Join-Path $proj_root Chalice2SIL\silast\src\SILAST;

if($SkipCompile){
    $compileCmd = "";
}else{
    $compileCmd = "compile";
}

$sd = Get-ScriptDirectory;
echo $sd;

$sbt = Join-Path $sd "chalice2sil\sbt.ps1";

Push-Location $chalice_root
    echo "================== COMPILING CHALICE =============================="
    & $sbt "set scalaVersion := \`"2.9.1\`"" $compileCmd package #scala version needs to be escaped twice
    echo "================== DONE COMPILING CHALICE ========================="
Pop-Location

Push-Location $silast_root
    echo "================== COMPILING SILAST ==============================="
    & $sbt $compileCmd package
    echo "================== DONE COMPILING SILAST =========================="
Pop-Location

$chalice_jar = Join-Path $chalice_root target\scala-2.9.1\chalice_2.9.1-1.0.jar
$silast_jar  = Join-Path $silast_root  target\scala-2.9.1\silast_2.9.1-0.1.jar

$lib_dir = "chalice2sil\lib";

if(-not (Test-Path $lib_dir)){
    New-Item -Type Container -Path $lib_dir | Out-Null;
}

cp $CHALICE_JAR (Join-Path $lib_dir "chalice.jar")
cp $SILAST_JAR  (Join-Path $lib_dir "silast.jar")

