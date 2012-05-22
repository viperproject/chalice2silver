# Chalice2SIL does not stand alone. 
# It builds on Chalice (1) to create a SILAST (2) 
# that it can then feed to Silicon (3) for verification.
#
# This script clones Boogie (of which Chalice is a part),
# SILAST and Silicon into subfolders of Chalice2SIL
# in order to use them as SBT sub-projects for building
# Chalice2SIL

$ErrorActionPreference = 'Stop';

# Get the directory the script resides in
$Invocation = (Get-Variable MyInvocation -Scope 0).Value;
$sd = (Split-Path $Invocation.MyCommand.Path);

# Clone Boogie (Chalice is part of that repository)
$boogie = Join-Path $sd "boogie";
if(-not (Test-Path $boogie)){
    "Cloning Boogie to $boogie"
    hg clone https://hg.codeplex.com/boogie "$boogie" --quiet
    $chalice = Join-Path $boogie "Chalice";
    $patch = Join-Path $sd "chalice-scala29.patch";
    $sbtjar = Join-Path $sd .\sbt-launch.jar;
    $sbt = Join-Path $sd .\sbt.ps1;    
    hg --repository $boogie import --no-commit "$patch"
    cp $sbtjar,$sbt $chalice;
}



# Check out SILAST
$silast = Join-Path $sd "silast";
if(-not (Test-Path $silast)){
    "Checking out SILAST to $silast"
    svn checkout https://svn.inf.ethz.ch/svn/pmueller/pm-group/projects/semper/SILAST/trunk "$silast" --quiet
}

# Check out Silicon
$silicon = Join-Path $sd "silicon";
if(-not (Test-Path silicon)){
    "Checking out Silicon to $silicon"
    svn checkout https://svn.inf.ethz.ch/svn/pmueller/pm-group/projects/semper/SymbExEngine/trunk/Silicon "$silicon" --quiet
}

