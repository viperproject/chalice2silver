@echo off
SetLocal

set THIS_DIR=%~dp0
set BASE_DIR=%THIS_DIR%\..\..
set ZIP_EXE=zip

pushd %THIS_DIR%

call assembly.bat

mkdir examples

REM xcopy /S %BASE_DIR%\src\test\resources\*.* examples

REM del /s examples\*.bat
REM rmdir /S /Q examples\quantificationOverPermissions
REM rmdir /S /Q examples\chaliceSuite\substantial-examples

unzip examples.zip -d examples\.

REM http://www.info-zip.org/Zip.html
zip -r chalice2sil.zip chalice2sil.jar chalice2sil.bat examples

del chalice2sil.jar
rmdir /S /Q examples

popd
exit /B 0