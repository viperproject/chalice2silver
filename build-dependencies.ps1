# Get the directory the script resides in
$Invocation = (Get-Variable MyInvocation -Scope 0).Value;
$sd = (Split-Path $Invocation.MyCommand.Path);

# Assemble paths
$chalice = Join-Path $sd "..\boogie\Chalice";
$silicon = Join-Path $sd "..\silicon";
$silast = Join-Path $sd "..\silast\src\SILAST";

function publish-local($dir){
    Push-Location $dir;
    try {
        .\sbt.ps1 publish-local
    } catch {
        Write-Error $_.Exception;
        Write-Error "Failed to build $dir.";
    } finally {
        Pop-Location;
    }
}

publish-local $chalice;
publish-local $silast;
publish-local $silicon;
