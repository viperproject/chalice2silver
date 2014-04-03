@echo off
SetLocal EnableDelayedExpansion

set THIS_DIR=%~dp0
set BASE_DIR=%THIS_DIR%..\..\

pushd %THIS_DIR%

pushd %BASE_DIR%
call sbt assembly
popd

copy %BASE_DIR%\target\scala-2.10\chalice2sil.jar %THIS_DIR%\.

call %THIS_DIR%\chalice2sil.bat -v

popd
exit /B 0