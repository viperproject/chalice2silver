$__chalice2sil_env_sd = Split-Path (Get-Variable MyInvocation -Scope 0).Value.MyCommand.Path;

function Run-Chalice(
	[Parameter()][String] $Chalice = '..\boogie\Chalice', 
	[Parameter()][Switch] $WhatIf, 
	[Parameter(Position=0)] $firstArg = "-help",
	[Parameter()][String] $File = '',
	[Parameter(ValueFromRemainingArguments = $true)] $remainingArgs){

	$ErrorActionPreference = "Stop";

	$sd = $__chalice2sil_env_sd;

	$sbt = Join-Path $sd "sbt.ps1";
	
	if($firstArg -ne $null){
		$remainingArgs = (,$firstArg) + $remainingArgs;
	}
	
	if($File -ne $null){
		$f = Get-Item $File;
		if($f -eq $null) {echo "OMG";}
		$remainingArgs = $remainingArgs + (,$f);
	} else {
		$remainingArgs = $remainingArgs;
	}
	
	if($remainingArgs -eq $null){
		$remainingArgs = @();
	}
	
	$sargs = $remainingArgs | where {$_ -ne $null} | foreach { $s = $_.ToString().Replace("\","\\").Replace("`"","\`""); "$s" }
	$eargs = [String]::Join(" ", $sargs)
	
	if($WhatIf){
		echo $eargs
		echo "$sbt `"run $eargs`""
	} else {
        $ErrorActionPreference = "Continue";
		pushd $Chalice;
		& $sbt "run-main chalice.Chalice $eargs"
		if(Test-Path .\out.bpl){
			cp .\out.bpl $sd\out.bpl -Force
		}
		popd
	}
}

function Run-Chalice2SIL([Parameter()][Switch] $WhatIf, [Parameter(Position=0)] $firstArg = "-help",
	[Parameter(ValueFromRemainingArguments = $true)] $remainingArgs){

	$sd = $__chalice2sil_env_sd;

	$sbt = Join-Path $sd "sbt.ps1";
	$sargs = ((,$firstArg) + $remainingArgs) | where {$_ -ne $null} | foreach { $s = $_.Replace("\","\\").Replace("`"","\`""); "$s" }
	$eargs = [String]::Join(" ", $sargs)

	if($WhatIf){
		echo $eargs
		echo "$sbt `"run $eargs`""
	} else {
		& $sbt "run $eargs"
	}
}