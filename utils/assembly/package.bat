@echo off
SetLocal

set THIS_DIR=%~dp0
set BASE_DIR=%THIS_DIR%\..\..
set ZIP_EXE=zip

pushd %THIS_DIR%

call assembly.bat

mkdir examples

xcopy /S %BASE_DIR%\src\test\resources\*.* examples

del /s examples\*.bat
rmdir /S /Q examples\quantificationOverPermissions
rmdir /S /Q examples\chaliceSuite\substantial-examples

REM http://www.info-zip.org/Zip.html
zip -r chalice2sil.zip chalice2sil.jar chalice2sil.bat examples

del chalice2sil.jar
rmdir /S /Q examples

popd
exit /B 0