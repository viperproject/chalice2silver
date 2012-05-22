param([Switch] $Force)

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

# Assemble paths
$boogie = Join-Path $sd "boogie";
$silast = Join-Path $sd "silast";
$silicon = Join-Path $sd "silicon";

if($Force){
	Remove-Item $boogie,$silast,$silicon -Recurse -Force;
}

# Clone Boogie (Chalice is part of that repository)

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

if(-not (Test-Path $silast)){
    "Checking out SILAST to $silast"
    svn checkout https://svn.inf.ethz.ch/svn/pmueller/pm-group/projects/semper/SILAST/trunk "$silast" --quiet
}

# Check out Silicon

if(-not (Test-Path silicon)){
    "Checking out Silicon to $silicon"
    svn checkout https://svn.inf.ethz.ch/svn/pmueller/pm-group/projects/semper/SymbExEngine/trunk/Silicon "$silicon" --quiet
}

#########################################################################################
########     TROUBLESHOOTING                                         ####################
#########################################################################################
#
# 1. What does 'scala.tools.nsc.MissingRequirementError: object scala not found.' mean?
#
# The, or rather 'a', scala library is missing. This might be due to an old copy 
# of silast.jar or chalice.jar in a /lib directory. Make sure that SBT manages the
# inter-dependencies, i.e., delete silast.jar and chalice.jar from /lib.
#
#
#

